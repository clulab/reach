package org.clulab.reach

/**
  * This object wraps the taxonomy
  */
import java.util.Collection
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import org.clulab.odin.impl.Taxonomy


object ReachConstants {

  // Could be used by other components
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

  val ACTIVATION_EVENTS = taxonomy.hyponymsFor("ActivationEvent").toSet
  val REGULATION_EVENTS = taxonomy.hyponymsFor("Regulation").toSet
  val COMPLEX_EVENTS = taxonomy.hyponymsFor("ComplexEvent").toSet

  val ADDITION_EVENTS = taxonomy.hyponymsFor("AdditionEvent").toSet
  val REMOVAL_EVENTS = taxonomy.hyponymsFor("RemovalEvent").toSet
  val MODIFICATION_EVENTS = ADDITION_EVENTS ++ REMOVAL_EVENTS
  val SIMPLE_EVENTS = taxonomy.hyponymsFor("SimpleEvent").toSet

}

