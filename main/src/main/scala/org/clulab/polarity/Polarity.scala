package org.clulab.polarity

/**
  * Base class to polarity. It is meant to be used as an enumeration
  */
abstract class Polarity

/**
  * Singleton to be used as a type-safe representation of positive polarity
  */
case object PositivePolarity extends Polarity

/**
  * Singleton to be used as a type-safe representation of negative polarity
  */
case object NegativePolarity extends Polarity

/**
  * Singleton to be used in case polarity doesn't apply
  */
case object NeutralPolarity extends Polarity
