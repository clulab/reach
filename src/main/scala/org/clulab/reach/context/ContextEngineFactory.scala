package org.clulab.reach.context

import java.io.File
import org.clulab.reach.conetxt.ml.LinearContextEngine


object ContextEngineFactory {

    // Enumeration with the type of context engines
    object Engine extends Enumeration {
      type Engine = Value
      val Dummy = Value("Dummy")
      val Policy1 = Value("Policy1")
      val Policy2 = Value("Policy2")
      val Policy3 = Value("Policy3")
      val Policy4 = Value("Policy4")
      val Linear = Value("Linear")
    }
    import Engine._

    def buildEngine(kind:Engine, params:Map[String, String]):ContextEngine = {

        val bound:Option[Int] = params.lift("bound") match {
            case Some(b) => Some(b.toInt)
            case None => None
        }

        val model:Option[File] = params.lift("model_path") match {
          case Some(p) => Some(new File(p))
          case None => None
        }

        val normalizers:Option[File] = params.lift("normalizers_path") match {
          case Some(p) => Some(new File(p))
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
            case Linear => (model, normalizers) match {
              case (Some(m), Some(n)) => new LinearContextEngine(m, n)
              case _ => throw new RuntimeException("Need to provide model and normalizers paths for the context engine")
            }
            case Dummy => new DummyContextEngine
            case _ => new DummyContextEngine
        }
    }
}
