package org.clulab

import java.util.Collection
import org.clulab.odin.impl.Taxonomy
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

/**
  * Class to run Reach reading and assembly then produce FRIES format output
  *   Written by: Gus Hahn-Powell. 7/16/2016.
  *   Last Modified: Add file input pattern.
  */
package object reach {

  /** Pattern identifying files which Reach can potentially process. */
  val ReachInputFilePattern = """.*\.(nxml|csv|tsv)$"""

  // Taxonomy object
  val taxonomy = readTaxonomy("org/clulab/reach/biogrammar/taxonomy.yml")

  private def readTaxonomy(path: String): Taxonomy = {
    val url = getClass.getClassLoader.getResource(path)
    val source = if (url == null) io.Source.fromFile(path) else io.Source.fromURL(url)
    val input = source.mkString
    source.close()
    val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[Collection[Any]]
    Taxonomy(data)
  }
}
