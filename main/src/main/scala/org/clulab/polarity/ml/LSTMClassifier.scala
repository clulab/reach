import org.clulab.polarity.Polarity
import org.clulab.polarity.ml.PolarityClassifier
import org.clulab.reach.mentions.BioEventMention
import scala.collection.mutable.ListBuffer
import edu.cmu.dynet._
import scala.io.Source
import scala.util.Random

class LSTMClassifier extends PolarityClassifier {
  Initialize.initialize()

  val dictPath = "vocab.txt"
  val w2vDictPath = "w2vvoc.txt"

  val lines = Source.fromFile(dictPath).getLines().toList
  val lines2 = Source.fromFile(w2vDictPath).getLines().toList

  val missing_voc = lines.zipWithIndex.toMap
  val w2v_voc = lines2.zipWithIndex.toMap

  val VOC_SIZE = 3671
  val WEM_DIMENSIONS = 100
  val NUM_LAYERS = 1
  val HIDDEN_SIZE = 30
  val N_EPOCH = 5

  var loss: Float = 0
  val pc = new ParameterCollection
  val w2v_wemb_lp: LookupParameter = pc.addLookupParameters(1579375, /*1579375,*/ Dim(Seq(WEM_DIMENSIONS)))

  val builder = new LstmBuilder(NUM_LAYERS, WEM_DIMENSIONS, HIDDEN_SIZE, pc)

  val p_W = pc.addParameters(Dim(1, HIDDEN_SIZE))
  val p_b = pc.addParameters(Dim(1))

  val sgd = new SimpleSGDTrainer(pc)

  val missing_vec = new FloatVector(WEM_DIMENSIONS)



  /**
    *
    * @param words A sinlge input sentence that is converted to an Array of String
    * @return y_pred The predicted value as an expression
    */
  def model_forward(words:Seq[String]):Expression = {

    val W = Expression.parameter(p_W)
    val b = Expression.parameter(p_b)

    // set input expression in computation graph
    //val x_1 = Expression.input(Dim(INPUT_SIZE), x_values)
    //val x_2 = Expression.input(Dim(INPUT_SIZE), x_values)
    //val x_3 = Expression.input(Dim(INPUT_SIZE), x_values)
    //val inputs = List(x_1, x_2, x_3)
    val inputs = words map { word =>

      w2v_voc.contains(word) match {
        case true =>
          Expression.lookup(w2v_wemb_lp, w2v_voc(word))
        case false =>
          Expression.input(Dim(WEM_DIMENSIONS), missing_vec)
        //          Expression.lookup(missing_wemb_lp, missing_voc(word))
      }
    }

    // set target expression in computation graph

    //println(W.dim())
    //println(b.dim())

    builder.newGraph() // map here is like the [f(i) for i in list] in python
    builder.startNewSequence()
    val states = inputs map { // transducer
      w => builder.addInput(w)
    }


    // Get the last embedding
    val selected = states.last

    // Run the FF network for classification
    Expression.logistic(W * selected + b)

  }

  def read_from_spreadsheet(spreadsheet_path:String, train_ratio:Float): (Seq[Seq[String]], Seq[Int], Seq[Seq[String]], Seq[Int]) ={

    var sentences = Seq[Seq[String]]()
    var labels = Seq[Int]()

    val bufferedSource = Source.fromFile(spreadsheet_path)
    for (line <- bufferedSource.getLines.drop(1)) {
      val cols = line.split("\t").map(_.trim)
      // do whatever you want with the columns here
      val sentence = cols(0)
      val start = cols(2).toInt
      val end = cols(3).toInt

      sentences = sentences :+ sentence.split(' ').slice(start, end).toSeq
      if (cols(5).startsWith("Pos")) {
        val label = 1
        labels = labels :+label
      }else{
        val label=0
        labels = labels :+label
      }
    }
    bufferedSource.close

    println(s"Num. all samples: ${sentences.length}")
    println(s"Num. all labels: ${labels.length}")


    val random_1 = new Random(1)
    val random_2 = new Random(1)

    val sens_shuffled = random_1.shuffle(sentences)
    val labels_shuffle = random_2.shuffle(labels)

    val n_training = (labels.length * train_ratio).toInt

    val sens_train = sens_shuffled.slice(0, n_training)
    val labels_train = labels_shuffle.slice(0, n_training)
    val sens_test = sens_shuffled.slice(n_training, labels.length)
    val labels_test = labels_shuffle.slice(n_training, labels.length)


    (sens_train, labels_train, sens_test, labels_test)

  }


  /**
    * Trains the classifier. This method is meant to have side effects by fitting the parameters
    *
    * @param events Training data
    * @param labels Training labels
    */
  override def fit(events: Seq[BioEventMention], labels: Seq[Polarity]): Unit = ???


  /**
    * For internal use only. Train the network on the spreadsheet
    * @param inputs: a Sequence of sentences
    * @param labels: a sequences of labels
    */
  def fit_(input_sens: Seq[Seq[String]],labels: Seq[Int]): Unit = {


    var total_loss = 0.toFloat
    for ((words, label) <- input_sens zip labels) {
      ComputationGraph.renew()


      val y_value = label

      val y = Expression.input(y_value)
      val y_pred = model_forward(words)
      val loss_expr = Expression.binaryLogLoss(y_pred, y)
      loss = ComputationGraph.forward(loss_expr).toFloat
      ComputationGraph.backward(loss_expr)
      sgd.update()
      total_loss+=loss

    }
    var average_loss = total_loss/input_sens.length
    println(s"training loss ${average_loss}")

  }

  def test_(input_sens: Seq[Seq[String]],labels: Seq[Int]): Unit = {

    var total_loss = 0.toFloat
    var correct_count = 0
    for ((words, label) <- input_sens zip labels) {
      ComputationGraph.renew()


      val y_value = label

      val y = Expression.input(y_value)
      val y_pred = model_forward(words)
      val loss_expr = Expression.binaryLogLoss(y_pred, y)
      loss = ComputationGraph.forward(loss_expr).toFloat
      total_loss+=loss

      if (y_pred.value().toFloat>0.5){
        if (label==1) {correct_count+=1}
      }
      else{
        if (label==0) {correct_count+=1}
      }

    }
    var average_loss = total_loss/input_sens.length
    var test_acc = correct_count.toFloat/labels.length
    println(s"testing loss ${average_loss}")
    println(s"testing acc ${test_acc}")
  }

    /**
      * Returns whether fit has been called before. Mostly for control
      *
      * @return True if the parameters of the model have been fit previously
      */

  override def isFitted: Boolean = ???

  /**
    * Gets the predicted polarity for the provided argument
    *
    * @param events Ordered sequence of events to get their polarity from
    * @return Predictions of Polarity subclasses
    */
  override def predict(events: Seq[BioEventMention]): Seq[Polarity] = ???

  /**
    * Saves the model parameter's to a file
    *
    * @param modelPath file path to save the model to.
    */
  override def save(modelPath: String): Unit = ???
}

object LSTMClassifier extends App {


  /**
    * TODO: save the parameters so that later the model can be used to predict the polarity.
    */
  val lstmClassifier = new LSTMClassifier()

  val spreadsheet_path = "SentencesInfo_all_label_final_ExactRecur.txt"
  val train_ratio=(0.8).toFloat
  val (sens_train, labels_train, sens_test, labels_test) = lstmClassifier.read_from_spreadsheet(spreadsheet_path, train_ratio)
  /*
  val sentence_1 = Seq[String]("I","have","a","dream")
  val sentence_2 = Seq[String]("I","have","a","dream","2")
  val sentences = Seq[Seq[String]](sentence_1, sentence_2)


  val labels = Seq[Int](0,1)
  */
  val N_EPOCH = lstmClassifier.N_EPOCH
  for (epoch <- 1 to N_EPOCH) {
    println(s"epoch $epoch")
    lstmClassifier.fit_(sens_train, labels_train)
    lstmClassifier.test_(sens_test, labels_test)
  }


}
