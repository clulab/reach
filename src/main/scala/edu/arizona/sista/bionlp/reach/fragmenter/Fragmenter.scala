package edu.arizona.sista.bionlp.reach.fragmenter

import java.net.URL
import java.io.{File, FileInputStream, InputStream}
import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import org.biopax.paxtools.io.SimpleIOHandler
import org.biopax.paxtools.model.BioPAXElement
import org.biopax.paxtools.model.level3._

class Fragmenter(is: InputStream) {
  val handler = new SimpleIOHandler()
  val model = handler.convertFromOWL(is)
  is.close()

  lazy val interactions = getObjects[Interaction]
  lazy val controls = getObjects[Control]
  lazy val conversions = getObjects[Conversion]
  lazy val geneticInteractions = getObjects[GeneticInteraction]
  lazy val molecularInteractions = getObjects[MolecularInteraction]
  lazy val templateReactions = getObjects[TemplateReaction]

  // conversions
  lazy val biochemicalReactions = getObjects[BiochemicalReaction]
  lazy val complexAssemblies = getObjects[ComplexAssembly]
  lazy val degradations = getObjects[Degradation]
  lazy val transports = getObjects[Transport]
  lazy val transportWithBiochemicalReactions = getObjects[TransportWithBiochemicalReaction]

  // controls
  lazy val catalysis = getObjects[Catalysis]
  lazy val modulations = getObjects[Modulation]
  lazy val templateReactionRegulations = getObjects[TemplateReactionRegulation]

  lazy val unificationXrefs = getObjects[UnificationXref]

  def getObjects[T <: BioPAXElement: ClassTag] = {
    val cls = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
    model.getObjects(cls).asScala.toSet
  }

  def getEntityReferenceById(id: String) = unificationXrefs find (_.id == id) match {
    case Some(xref) => xref.xrefOf map (_.asInstanceOf[EntityReference])
    case None => Set.empty
  }

  def getByRdfId(id: String) = model getByID id match {
    case null => None
    case elem => Some(elem)
  }
}

object Fragmenter {
  def fromFile(file: File): Fragmenter = new Fragmenter(new FileInputStream(file))
  def fromFile(filename: String): Fragmenter = fromFile(new File(filename))
  def fromURL(url: URL): Fragmenter = new Fragmenter(url.openStream)
  def fromURL(spec: String): Fragmenter = fromURL(new URL(spec))
}

object FragmenterApp extends App {
  val filename = "/home/marcov/Downloads/Pathway Commons.4.Reactome.BIOPAX.owl"
  val frag = Fragmenter fromFile filename

  println(s"BiochemicalReactions = ${frag.biochemicalReactions.size}")
  println

  val hist = frag.biochemicalReactions groupBy (_.conversionLabels)
  hist foreach { case (labels, reactions) =>
    println(s"$labels = ${reactions.size}")
    reactions map ("  " + _.name) foreach println
    println
  }
}
