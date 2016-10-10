package org.clulab

import java.util.Collection
import org.clulab.odin.impl.Taxonomy
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor


package object reach {

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
