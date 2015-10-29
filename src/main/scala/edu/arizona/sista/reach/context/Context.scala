package edu.arizona.sista.reach.context

trait Context {
  var context: Option[Map[String, Seq[String]]] = None
}
