package org.clulab.reach.utils

// See https://stackoverflow.com/questions/2662020/can-i-create-a-collection-in-scala-that-uses-different-equals-hashcode-compare-i
class IdentBox[T <: AnyRef](val value: T) {

  override def equals(other: Any): Boolean = other match {
    case that: IdentBox[T] => that.value eq this.value
    case _ => false
  }

  override def hashCode(): Int = System.identityHashCode(value)
}
