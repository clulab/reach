package org.clulab

import java.util.Collection

import scala.io.Source

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import org.clulab.odin.impl.Taxonomy

/**
  * Class to run Reach reading and assembly then produce FRIES format output
  *   Written by: Gus Hahn-Powell. 7/16/2016.
  *   Last Modified: Fix file input pattern to include "txt".
  */
package object reach {

  /** Pattern identifying files which Reach can potentially process. */
  val ReachInputFilePattern = """.*\.(nxml|csv|tsv|txt|ser|xml)$"""

  // Taxonomy object
  val taxonomy = readTaxonomy("org/clulab/reach/biogrammar/taxonomy.yml")

  private def readTaxonomy(path: String): Taxonomy = {
    val url = getClass.getClassLoader.getResource(path)
    val source = if (url == null) Source.fromFile(path) else Source.fromURL(url)
    val input = source.mkString
    source.close()
    val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[Collection[Any]]
    Taxonomy(data)
  }
}
