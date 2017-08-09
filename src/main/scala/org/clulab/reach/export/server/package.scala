package org.clulab.reach.export

import scala.annotation.tailrec

package object server {

  // @throws(classOf[Exception])
  // @tailrec
  // def buildArgMap(map : Map[String, String], args: List[String]): Map[String, String] = args match {
  //   case Nil => map
  //   // handle port
  //   case "--port" :: port :: tail =>
  //     buildArgMap(map ++ Map("port" -> port), tail)
  //   case "-p" :: port :: tail =>
  //     buildArgMap(map ++ Map("port" -> port), tail)
  //   // handle host
  //   case "--host" :: host :: tail =>
  //     buildArgMap(map ++ Map("host" -> host), tail)
  //   case unknown :: tail =>
  //     throw new Exception(s"""Unknown option "$unknown"""")
  // }

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
