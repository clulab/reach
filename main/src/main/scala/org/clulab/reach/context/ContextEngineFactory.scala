package org.clulab.reach.context


object ContextEngineFactory {

    // Enumeration with the type of context engines
    object Engine extends Enumeration {
      type Engine = Value
      val Dummy = Value("Dummy")
      val Policy1 = Value("Policy1")
      val Policy2 = Value("Policy2")
      val Policy3 = Value("Policy3")
      val Policy4 = Value("Policy4")
      val SVMPolicy = Value("SVMPolicy")
    }
    import Engine._

    def buildEngine(kind:Engine, params:Map[String, String]):ContextEngine = {

        val bound:Option[Int] = params.lift("bound") match {
            case Some(b) => Some(b.toInt)
            case None => None
        }

        kind match {
            case Policy1 => new PaddingContext
            case Policy2 => bound match {
                case Some(b) => new BoundedPaddingContext(b)
                case None => new BoundedPaddingContext
            }
            case Policy3 => bound match {
                case Some(b) => new FillingContext(b)
                case None => new FillingContext
            }
            case Policy4 => bound match {
                case Some(b) => new BidirectionalPaddingContext(b)
                case None => new BidirectionalPaddingContext
            }
            case Dummy => new DummyContextEngine
            case SVMPolicy => bound match {
              case w @ Some(b) => new SVMContextEngine(w)
              case None => new SVMContextEngine
            }
            case _ => new DummyContextEngine
        }
    }
}
