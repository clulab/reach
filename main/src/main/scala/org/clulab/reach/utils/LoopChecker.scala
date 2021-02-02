package org.clulab.reach.utils

import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.collection.mutable.{HashSet => MutableHashSet}

abstract class LoopChecker[Value <: AnyRef] {
  type ValueKey = IdentBox[Value]
  type ParentSet = MutableHashSet[ValueKey]
  type ParentMap = MutableHashMap[ValueKey, ParentSet]

  protected val parentMap = new ParentMap()
  protected val emptyParentSet = new ParentSet()

  protected def getChildren(value: Value): Seq[Value]

  protected def checkForLoop(value: Value, newParents: ParentSet): Boolean = {
    val valueKey = new ValueKey(value)
    if (newParents.contains(valueKey))
      true
    else {
      val oldParentsOpt = parentMap.get(valueKey)
      if (oldParentsOpt.isDefined && newParents.subsetOf(oldParentsOpt.get))
        false
      else {
        val oldParents = oldParentsOpt.getOrElse {
          val oldParents = new ParentSet()
          // This is always a fresh copy, so newParents above can be a duplicate.
          parentMap(valueKey) = oldParents
          oldParents
        }
        oldParents ++= newParents
        val allParents = oldParents + valueKey
        val loopValueOpt = getChildren(value).find { child =>
          checkForLoop(child, allParents)
        }
        loopValueOpt.isDefined
      }
    }
  }

  def checkForLoops(values: Seq[Value]): Boolean = {
    val loopValueOpt = values.find { value =>
      // We know of no parents for these values at this point.
      checkForLoop(value, emptyParentSet)
    }
    loopValueOpt.isDefined
  }
}
