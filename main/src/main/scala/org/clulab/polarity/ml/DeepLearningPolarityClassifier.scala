package org.clulab.polarity.ml

import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.Expression._
import edu.cmu.dynet._
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser
import org.clulab.polarity.{NegativePolarity, NeutralPolarity, Polarity, PositivePolarity}
import org.clulab.reach.mentions.{BioEventMention, CorefEventMention}
import org.clulab.odin.{Mention, EventMention, TextBoundMention}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random



class DeepLearningPolarityClassifier() extends PolarityClassifier{

  Initialize.initialize()

  val config = ConfigFactory.load()

  val configPath = "polarity"
  var maskOption = "tag_name"
  var savedModelPath = "savedModel"
  var spreadsheetPath = "SentencesInfo_all_label_final_ExactRecur_ExpandBound.txt"
  var VOC_SIZE = 3671
  var WEM_DIMENSIONS = 100
  var CEM_DIMENSIONS = 30
  var NUM_LAYERS = 1
  var HIDDEN_SIZE = 30
  var MLP_HIDDEN_SIZE = 10
  var N_EPOCH = 5

  if(config.hasPath(configPath)) {
    maskOption = config.getString(configPath+".maskOption")
    savedModelPath = config.getString(configPath+".savedModel")+"_"+maskOption
    spreadsheetPath = config.getString(configPath+".spreadsheetPath")
    VOC_SIZE = config.getInt(configPath+".VOC_SIZE")
    WEM_DIMENSIONS = config.getInt(configPath+".WEM_DIMENSIONS")
    CEM_DIMENSIONS = config.getInt(configPath+".CEM_DIMENSIONS")
    NUM_LAYERS = config.getInt(configPath+".NUM_LAYERS")
    HIDDEN_SIZE = config.getInt(configPath+".HIDDEN_SIZE")
    MLP_HIDDEN_SIZE = config.getInt(configPath+".MLP_HIDDEN_SIZE")
    N_EPOCH = config.getInt(configPath+".N_EPOCH")
  }
  else{
    logger.error("Config file doesn't have polarity engine configured. Returning the default engine")
  }

  //val dictPath = "vocab.txt"
  //val w2vDictPath = "w2vvoc.txt"

  //val lines = Source.fromFile(dictPath).getLines().toList
  //val lines2 = Source.fromFile(w2vDictPath).getLines().toList

  val (w2i, c2i) = mkVocabs(spreadsheetPath)


  var loss: Float = 0
  val pc = new ParameterCollection
  val w2v_wemb_lp: LookupParameter = pc.addLookupParameters(w2i.size, /*1579375,*/ Dim(Seq(WEM_DIMENSIONS)))
  val c2v_cemb:LookupParameter = pc.addLookupParameters(c2i.size, /*1579375,*/ Dim(Seq(CEM_DIMENSIONS)))

  val p_W = pc.addParameters(Dim(1, 2*HIDDEN_SIZE+1))
  //val p_V = pc.addParameters(Dim(1, MLP_HIDDEN_SIZE))
  val p_b = pc.addParameters(Dim(1))
  //val p_bv = pc.addParameters(Dim(1))


  val builderFwd = new LstmBuilder(NUM_LAYERS, WEM_DIMENSIONS+CEM_DIMENSIONS*2, HIDDEN_SIZE, pc)
  val builderBwd = new LstmBuilder(NUM_LAYERS, WEM_DIMENSIONS+CEM_DIMENSIONS*2, HIDDEN_SIZE, pc)
  val charFwRnnBuilder = new LstmBuilder(NUM_LAYERS, CEM_DIMENSIONS, CEM_DIMENSIONS, pc)
  val charBwRnnBuilder = new LstmBuilder(NUM_LAYERS, CEM_DIMENSIONS, CEM_DIMENSIONS, pc)


  //val sgd = new SimpleSGDTrainer(pc)
  val sgd = new AdamTrainer(pc)
  if (maskOption=="tag") {
    sgd.clipThreshold = 4.0.toFloat
  }

  val missing_vec = new FloatVector(WEM_DIMENSIONS)
  val missing_charVec = new FloatVector(CEM_DIMENSIONS*2)

  private var _isFitted = false
  
  if (Files.exists(Paths.get(savedModelPath))){
    logger.info(s"Loading saved model $savedModelPath ...")
    _isFitted=true
    val modelLoader = new ModelLoader(savedModelPath)
    modelLoader.populateModel(pc, "/allParams")
    logger.info("Loading model finished!")
  }
  else{
    logger.info("Could not find specified model. ")
  }
  
  

  /**
    * Trains the classifier. This method is meant to have side effects by fitting the parameters
    *
    * @param trainingPath Training data
    * @param trainingRatio Ratio of training samples in the dataset
    */
  override def fit(trainingPath:String = spreadsheetPath, trainRatio:Float=0.8.toFloat, saveFlag:Boolean=true): Unit = {
    
    if (!_isFitted){
      val (sens_train, labels_train, sens_test, labels_test) = this.readFromSpreadsheet(trainingPath, trainRatio, maskOption)
      val N_EPOCH = this.N_EPOCH
      for (epoch <- 1 to N_EPOCH) {
        logger.info(s"epoch $epoch")
        this.fitSingleEpoch(sens_train, labels_train)
        this.testSingleEpoch(sens_test, labels_test)
        if (maskOption=="tag") {
          sgd.learningRate=sgd.learningRate*0.6.toFloat
        }
        else{
          sgd.learningRate=sgd.learningRate*0.3.toFloat
        }
      }
      if (saveFlag) {save()}
      _isFitted=true
    }

  }

  /**
    * Returns whether fit has been called before. Mostly for control
    *
    * @return True if the parameters of the model have been fit previously
    */
  override def isFitted: Boolean = _isFitted

  /**
    * Gets the predicted polarity for the provided argument
    *
    * @param events Ordered sequence of events to get their polarity from
    * @return Predictions of Polarity subclasses
    */
  override def predict(events: Seq[BioEventMention]): Seq[Polarity] = {
    //println("++++++++++++++++++++++++++++++++++++++++++")
    var predictions = Seq[Polarity]()
    for (event<-events) {
      predictions = predictions:+predict(event)
    }
    predictions
  }

  override def predict(event: BioEventMention): Polarity = {
    if (event matches "ComplexEvent") {

//      println("==========================================")
//      println(s"Original text:${event.text}")
//      println(s"Unexpanded text:${event.sentenceObj.words.slice(event.start, event.end).toList.toString}")

      //var lemmas = event.lemmas.get.toArray
      var lemmas = event.sentenceObj.words.clone()
      //var lemmas = event.sentenceObj.lemmas.get.clone()
      val rule = event.label
      var rulePolarity = 0
      if (rule.startsWith("Neg")) {
        rulePolarity = 0
      } else {
        rulePolarity = 1
      }

      val controller = event.arguments("controller").isInstanceOf[mutable.ArraySeq] match {
        case true => event.arguments("controller").head // In some cases the controller is a vector, thus having no head.
      }
      val controlled = event.arguments("controlled").isInstanceOf[mutable.ArraySeq] match {
        case true => event.arguments("controlled").head // In some cases the controller is a vector, thus having no head.
      }

      //val ctrlr_start = controller.start
      //val ctrlr_end = controller.end

      lemmas = controller match {
        case controller:CorefEventMention => lemmas
        case controller:EventMention => maskRecursively(lemmas, controller, "controller")
        case controller:TextBoundMention => maskDirect(lemmas, maskOption, "controller", controller.start, controller.end)
      }

      lemmas = controlled match {
        case controlled:CorefEventMention => lemmas
        case controlled:EventMention => maskRecursively(lemmas, controlled, "controlled")
        case controlled:TextBoundMention => maskDirect(lemmas, maskOption, "controlled", controlled.start, controlled.end)
      }

      val (start, end) = getExpandBound(event, controller.start, controlled.start)

      println(lemmas.slice(start, end).toSeq)


      ComputationGraph.renew()

      val y_pred = runInstance(lemmas.slice(start, end), rulePolarity)

//      scala.io.StdIn.readLine()

      if (y_pred.value().toFloat > 0.5) {
        PositivePolarity
      }
      else {
        NegativePolarity
      }
    }
    else {NeutralPolarity}
  }

  def maskRecursively(lemmas:Array[String], mention:Mention, role:String):Array[String] = {
    lemmas
  }

  def maskDirect(lemmas:Array[String], maskOption:String, role:String, intervalStart:Int, intervalEnd:Int) : Array[String]= {
    if (role=="controller"){
      if (maskOption == "tag_name") {
        for (index <- intervalStart until intervalEnd) {
          lemmas(index) = "controller_" + lemmas(index)
        }
      }
      else if (maskOption == "tag") {
        for (index <- intervalStart until intervalEnd) {
          if (lemmas(index).endsWith("kd")) {
            lemmas(index) = "__controller__-kd"
          }
          else {
            lemmas(index) = "__controller__"
          }
        }
      }
    }
    if (role=="controlled"){
      if (maskOption == "tag_name") {
        for (index <- intervalStart until intervalEnd) {
          lemmas(index) = "controlled_" + lemmas(index)
        }
      }
      else if (maskOption == "tag") {
        for (index <- intervalStart until intervalEnd) {
          if (lemmas(index).endsWith("kd")) {
            lemmas(index) = "__controlled__-kd"
          }
          else {
            lemmas(index) = "__controlled__"
          }
        }
      }
    }
    lemmas
  }

  def predictManual(event:String, rulePolarity:Int): Unit= {
    val words  = event.split(" ")
    val y_pred = runInstance(words, rulePolarity)
    val y_pred_float = y_pred.value().toFloat().toString

    println(s"Output:${y_pred_float},  text: ${event}")
  }

  /**
    * Saves the model parameter's to a file
    *
    * @param modelPath file path to save the model to.
    */
  override def save(modelPath: String=savedModelPath): Unit = {
    logger.info("Saving model ...")
    new CloseableModelSaver(modelPath).autoClose { modelSaver =>
      modelSaver.addModel(pc, "/allParams")
    }
    logger.info("Saving trained model finished!")

  }

  def runInstance(words:Seq[String], rulePolarityNum:Int):Expression= {
    val W = Expression.parameter(p_W)
    //val V = Expression.parameter(p_V)
    val bw = Expression.parameter(p_b)
    //val bv = Expression.parameter(p_bv)

    val rulePolarity = Expression.input(rulePolarityNum)

    val inputsFwd = words map { word =>
      mkEmbedding(word.toLowerCase())
    }
    val inputsBwd = inputsFwd.reverse


    val statesFwd = transduce(inputsFwd, builderFwd)
    val statesBwd = transduce(inputsBwd, builderBwd)


    // Get the last embedding
    val selected = concatenate(statesFwd.last, statesBwd.last)
    val feedForwardInput = concatenate(selected, rulePolarity)

    // Run the FF network for classification
    //Expression.logistic(V * Expression.tanh(W * feedForwardInput + bw)+bv)
    Expression.logistic(W * feedForwardInput + bw)

  }

  def transduce(embeddings:Iterable[Expression], builder:RnnBuilder): Iterable[Expression] = {
    builder.newGraph()
    builder.startNewSequence()
    val states = embeddings.map(builder.addInput)
    states
  }

  def mkEmbedding(word: String):Expression = {
    //
    // make sure you preprocess the word similarly to the embedding library used!
    //   GloVe large does not do any preprocessing
    //   GloVe small lowers the case
    //   Our Word2Vec uses Word2Vec.sanitizeWord
    //
    val sanitized = word // word.toLowerCase() // Word2Vec.sanitizeWord(word)

    val wordEmbedding =
      if (w2i.contains(word)){
        Expression.lookup(w2v_wemb_lp, w2i(word))
      }
      else {
        Expression.input(Dim(WEM_DIMENSIONS), missing_vec)
      }

    // biLSTM over character embeddings
    val charEmbedding =
      mkCharEmbedding(word)

    concatenate(wordEmbedding, charEmbedding)
  }

  def mkCharEmbedding(word: String): Expression = {
    //println(s"make embedding for word [$word]")
    val charEmbeddings = new ArrayBuffer[Expression]()
    if (word.length>0){
      for(i <- word.indices) {
        if(c2i.contains(word.charAt(i))){
          charEmbeddings += Expression.lookup(c2v_cemb, c2i(word.charAt(i)))
        }
      }
      if (charEmbeddings.length>0){
        val fwOut = transduce(charEmbeddings, charFwRnnBuilder).last
        val bwOut = transduce(charEmbeddings.reverse, charBwRnnBuilder).last
        concatenate(fwOut, bwOut)
      }
      else{
        Expression.input(Dim(CEM_DIMENSIONS*2), missing_vec)
      }
    }
    else{
      Expression.input(Dim(CEM_DIMENSIONS*2), missing_vec)
    }
  }

  def charToIndex(w2v_voc:Map[String, Int], special_voc:Map[String, Int]):Map[Char,Int]={
    logger.info("Generating character embedding index ...")
    val chars = new mutable.HashSet[Char]()
    for (keyWord <- w2v_voc.keys) {
      for(i <- keyWord.indices) {
        chars += keyWord.charAt(i)
      }
    }
    for (keyWord <- special_voc.keys) {
      for(i <- keyWord.indices) {
        chars += keyWord.charAt(i)
      }
    }

    val c2i = chars.toList.sorted.zipWithIndex.toMap
    logger.info("Character index finished!")
    c2i
  }

  def fitSingleEpoch(input_sens: Seq[(Seq[String],Int)],labels: Seq[Int]): Unit = {
    var total_loss = 0.toFloat
    for ((instance, label) <- input_sens zip labels) {

      ComputationGraph.renew()

      val y_value = label

      val y = Expression.input(y_value)
      val y_pred = runInstance(instance._1, instance._2)
      val loss_expr = Expression.binaryLogLoss(y_pred, y)
      loss = ComputationGraph.forward(loss_expr).toFloat
      ComputationGraph.backward(loss_expr)
      sgd.update()
      total_loss+=loss

    }
    var average_loss = total_loss/input_sens.length
    logger.info(s"training loss ${average_loss}")

  }

  def testSingleEpoch(input_sens: Seq[(Seq[String],Int)],labels: Seq[Int]): Unit = {

    var total_loss = 0.toFloat
    var correct_count = 0
    var predLabels = Seq[Int]()
    for ((instance, label) <- input_sens zip labels) {
      ComputationGraph.renew()

      val y_value = label

      val y = Expression.input(y_value)
      val y_pred = runInstance(instance._1, instance._2)
      val loss_expr = Expression.binaryLogLoss(y_pred, y)
      loss = ComputationGraph.forward(loss_expr).toFloat
      total_loss+=loss

      if (y_pred.value().toFloat>0.5){
        predLabels = predLabels:+1
        if (label==1) {correct_count+=1}
      }
      else{
        predLabels = predLabels:+0
        if (label==0) {correct_count+=1}
      }

    }
    val average_loss = total_loss/input_sens.length
    val test_acc = correct_count.toFloat/labels.length
    val (precision, recall, f1)  = getPrecisionRecallF1(predLabels, labels)

    logger.info(s"number of testing samples ${labels.length}")
    logger.info(s"testing loss ${average_loss}")
    logger.info(s"testing acc ${test_acc}")
    logger.info(s"precision:${precision}\trecall:${recall}\tf1:${f1}")
  }

  def loadModelEval(trainingPath:String = spreadsheetPath, trainRatio:Float=0.8.toFloat):Unit={
    val (sens_train, labels_train, sens_test, labels_test) = this.readFromSpreadsheet(trainingPath, trainRatio, maskOption)

    this.testSingleEpoch(sens_test, labels_test)
  }

  def readFromSpreadsheet(spreadsheet_path:String, train_ratio:Float, mask_option:String="tag_name"): (Seq[(Seq[String], Int)], Seq[Int], Seq[(Seq[String], Int)], Seq[Int]) ={
    logger.info("Loading data from spreadsheet ...")
    var instances = Seq[(Seq[String],Int)]()
    var labels = Seq[Int]()

    val bufferedSource = Source.fromFile(spreadsheet_path)
    for (line <- bufferedSource.getLines.drop(1)) {

      val cols = line.split("\t").map(_.trim)
      // do whatever you want with the columns here
      val sentence = cols(0)
      val start = cols(2).toInt
      val end = cols(3).toInt

      var rulePolarity=0
      if (cols(5).startsWith("Pos")) {
        rulePolarity=1
      }else{
        rulePolarity=0
      }

      if (mask_option=="tag_name") {
        val ctrlr_start = cols(14).toInt
        val ctrlr_end = cols(15).toInt
        val ctrld_start = cols(16).toInt
        val ctrld_end = cols(17).toInt

        var sentence_mod = sentence.split(" ")
        for (index <- ctrlr_start until ctrlr_end){
          sentence_mod(index) = "controller_"+sentence_mod(index)
        }
        for (index <- ctrld_start until ctrld_end){
          sentence_mod(index) = "controlled_"+sentence_mod(index)
        }
        //println(sentence_mod.slice(start, end).toSeq)
        //scala.io.StdIn.readLine()
        instances = instances :+ (sentence_mod.slice(start, end).toSeq, rulePolarity)
      }
      else if (mask_option=="tag"){
        val ctrlr_start = cols(14).toInt
        val ctrlr_end = cols(15).toInt
        val ctrld_start = cols(16).toInt
        val ctrld_end = cols(17).toInt

        var sentence_mod = sentence.split(" ")
        for (index <- ctrlr_start until ctrlr_end){
          if (sentence_mod(index).toLowerCase().endsWith("kd")){
            sentence_mod(index) = "__controller__-kd"
          }
          else{sentence_mod(index) = "__controller__"}
        }
        for (index <- ctrld_start until ctrld_end){
          if (sentence_mod(index).toLowerCase().endsWith("kd")) {
            sentence_mod(index) = "__controlled__-kd"
          }
          else{sentence_mod(index) = "__controlled__"}
        }
//        println(sentence_mod.slice(start, end).toSeq)
//        scala.io.StdIn.readLine()
        instances = instances :+ (sentence_mod.slice(start, end).toSeq, rulePolarity)
      }
      else if (mask_option=="name"){
        instances = instances :+ (sentence.split(" ").slice(start, end).toSeq, rulePolarity)
      }
      if (cols(6).startsWith("Pos")) {
        val label = 1
        labels = labels :+label
      }else{
        val label=0
        labels = labels :+label
      }

    }
    bufferedSource.close

    logger.info(s"Num. all samples: ${instances.length}")
    logger.info(s"Num. all labels: ${labels.length}")


    val random_1 = new Random(1)
    val random_2 = new Random(1)

    val sens_shuffled = random_1.shuffle(instances)
    val labels_shuffle = random_2.shuffle(labels)

    //val sens_shuffled = sentences
    //val labels_shuffle = labels

    val n_training = (labels.length * train_ratio).toInt

    val sens_train = sens_shuffled.slice(0, n_training)
    val labels_train = labels_shuffle.slice(0, n_training)
    val sens_test = sens_shuffled.slice(n_training, labels.length)
    val labels_test = labels_shuffle.slice(n_training, labels.length)
    logger.info("Loading data finished!")



    (sens_train, labels_train, sens_test, labels_test)



  }

  def mkVocabs(spreadSheetPath:String): (Map[String, Int], Map[Char, Int]) = {
    logger.info("Making vocabulary for deep learning model ...")
    val (trainSentences, _, _,_) = readFromSpreadsheet(spreadSheetPath, 0.8.toFloat, maskOption)

    val chars = new mutable.HashSet[Char]()
    val words = new mutable.HashSet[String]()
    for(instance <- trainSentences) {
      for(token <- instance._1) {
        val word = token.toLowerCase
        words += word
        for(i <- word.indices) {
          chars += word.charAt(i)
        }
      }
    }

    val w2i = words.zipWithIndex.toMap
    val c2i = chars.toList.sorted.zipWithIndex.toMap

    logger.info(s"Vocabulary build finished! W2I size ${w2i.size},  C2I size ${c2i.size}")

    (w2i, c2i)
  }

  def getPrecisionRecallF1(predLabels:Seq[Int], trueLabels:Seq[Int]):(Float, Float, Float) = {
    // this computes the precision, recall and f1 for the positive class.
    val predLabelsSet  = predLabels.zipWithIndex.filter(_._1 == 1).map(_._2).toSet
    val trueLabelsSet = trueLabels.zipWithIndex.filter(_._1 == 1).map(_._2).toSet

    val truePositives = predLabelsSet.intersect(trueLabelsSet).size

    val precision = truePositives.toFloat/predLabelsSet.size.toFloat
    val recall = truePositives.toFloat/trueLabelsSet.size.toFloat
    val f1 = 2*precision*recall/(precision+recall)

    (precision, recall, f1)

  }

  def getExpandBound(event:BioEventMention, controller_start:Int, controlled_start:Int):(Int, Int) = {
    val event_start = event.start
    val event_end = event.end

    var event_start_new = event_start
    var event_end_new = event_end

    val dependencyTreeObj = event.document.sentences(0).dependencies.get.allEdges

    //println(dependencyTreeObj)

    for (edge <- dependencyTreeObj){
      val potentialBound = Seq(edge._1, edge._2)
      if (potentialBound.contains(controller_start)|| potentialBound.contains(controlled_start)){
        event_start_new = math.min(potentialBound.min, event_start_new)
        event_end_new = math.max(potentialBound.max, event_end_new)
      }
    }
    (event_start_new, event_end_new)
  }
}


// object DeepLearningPolarityClassifier extends App{
//   //def load(path:String):DeepLearningPolarityClassifier = ???
//   val lstmClassifier = new DeepLearningPolarityClassifier()
//   lstmClassifier.fit()
//   //lstmClassifier.loadModelEval()
// }
