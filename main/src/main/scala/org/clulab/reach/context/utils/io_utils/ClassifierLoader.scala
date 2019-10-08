package org.clulab.reach.context.utils.io_utils

import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.learning.LinearSVMClassifier

object ClassifierLoader {

  def getClassifierInstanceFromPath(pathToClassifier:String):LinearSVMContextClassifier ={
    val svmWrapper = new LinearSVMContextClassifier()
    val trainedSVMInstance = svmWrapper.loadFrom(pathToClassifier)
    trainedSVMInstance
  }
}
