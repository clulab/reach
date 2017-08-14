package org.clulab.reach.assembly

import scala.annotation.tailrec

package object server {

  @throws(classOf[Exception])
  @tailrec
  def buildServerArgMap (
    args: List[String],
    argMap: Map[String,String] = Map[String,String]()
  ): Map[String,String] = args match {
    case Nil => argMap                      // no more args
    case "--port" :: port :: tail =>        // port
      buildServerArgMap(tail, argMap ++ Map("port" -> port))
    case "-p" :: port :: tail =>            // port abbreviation
      buildServerArgMap(tail, argMap ++ Map("port" -> port))
    case "--host" :: host :: tail =>        // host
      buildServerArgMap(tail, argMap ++ Map("host" -> host))
    case "-h" :: host :: tail =>            // host abbreviation
      buildServerArgMap(tail, argMap ++ Map("host" -> host))
    case unknown :: tail =>                 // fail on unknown arguments
      throw new Exception(s"""Unknown option "$unknown"""")
  }
}
