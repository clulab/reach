package org.clulab.reach.assembly

import scala.annotation.tailrec


package object server {

  @throws(classOf[Exception])
  @tailrec
  def buildArgMap(map : Map[String, String], args: List[String]): Map[String, String] = args match {
    case Nil => map
    // handle port
    case "--port" :: port :: tail =>
      buildArgMap(map ++ Map("port" -> port), tail)
    case "-p" :: port :: tail =>
      buildArgMap(map ++ Map("port" -> port), tail)
    // handle host
    case "--host" :: host :: tail =>
      buildArgMap(map ++ Map("host" -> host), tail)
    case unknown :: tail =>
      throw new Exception(s"""Unknown option "$unknown"""")
  }
}
