package org.clulab.reach.context
import org.ml4ai.data.utils.correctDataPrep.{AggregatedRowNew, FoldMaker, Utils}
import org.ml4ai.data.classifiers.LinearSVMWrapper
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import scala.io.Source
object PerformCrossValReach extends App with LazyLogging {
  val svmWrapper = new LinearSVMWrapper(null)
  val config = ConfigFactory.load()
  val untrainedConfigPath = config.getString("contextEngine.params.untrainedSVMPath")
  val untrainedSVMInstance = svmWrapper.loadFrom(untrainedConfigPath)
  val foldsForSVMContextEngine = Source.fromFile(config.getString("contextEngine.params.folds"))
  val groupedPath = Some(Source.fromFile(config.getString("contextEngine.params.groupedFeatures")))
  val (_,rows) = AggregatedRowNew.fromStream(null, groupedPath)

  val foldsFromCSV = FoldMaker.getFoldsPerPaper(foldsForSVMContextEngine)
  val trainValCombined = Utils.combineTrainVal(foldsFromCSV)
  val filteredRows = rows.filter(_.PMCID != "b'PMC4204162'")
  val (truthTestSVM, predTestSVM) = FoldMaker.svmControllerLinearSVM(untrainedSVMInstance, trainValCombined, filteredRows)
  val svmResult = Utils.scoreMaker("Linear SVM", truthTestSVM, predTestSVM)
  logger.info(svmResult+" : results obtained by performing cross validation on old data in the reach pipeline")
}
