package org.clulab.reach.context.context_exec

object SVMCrossValidation extends App {

  // step 1: load trained model
  // step 2: load labels.csv i.e. "gold standard" for new annotated data
  // step 3: load aggregated rows written to file in polarityContext.aggrRowWrittenToFilePerPaperNewAnnotations
  // step 4: over each paper, find the prediction for the row. If it is 1, add to the list to find the micro-averaged precision


}
