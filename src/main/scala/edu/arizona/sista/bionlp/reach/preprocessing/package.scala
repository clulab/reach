package edu.arizona.sista.bionlp.reach

import java.io.File

import edu.arizona.sista.bionlp.reach.utils.FileReader
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.slf4j.LoggerFactory

/**
 * Created by gus
 */
package object preprocessing {

  type Lines = Seq[Array[String]]

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)
  val preprocessor = new BioNLPProcessor(withRuleNER = false, withCRFNER = false)

  // Retrieve relevant rule labels associated with a kb file basename
  val KBLUT: Map[String, Seq[String]] =
    Map("hmdb" -> Seq("Simple_chemical", "BioChemicalEntity"),
        "ProteinFamilies" -> Seq("Family", "BioChemicalEntity"),
        "tissue-type" -> Seq("Cellular_component", "BioChemicalEntity"),
        "uniprot-proteins" -> Seq("Gene_or_gene_product", "BioChemicalEntity"),
        "uniprot-subcellular-locations" -> Seq("Cellular_component", "BioChemicalEntity"),
        "GO-subcellular-locations" -> Seq("Cellular_component", "BioChemicalEntity"))
      .withDefaultValue(Seq("BioChemicalEntity"))

  val speciesOfInterest = Seq("Human", "Homo sapiens")

  val SpeciesLUT: Map[String, Seq[String]] =
    Map("uniprot-proteins" -> speciesOfInterest,
        "ProteinFamilies" -> speciesOfInterest).withDefaultValue(Nil)


  def mkLabelRepresentation(kb: String): String = "[" + KBLUT(kb).mkString(", ") + "]"

  /**
   * Only consider lines that match the set of valid species labels (defined in SpeciesLUT).
   * @param f a KB File
   * @return the valid lines pertaining to the specified species
   */
  def filterLines(f: File): Seq[String] = {
    val lines = FileReader.readFile(f).toSeq
    val basename = FileReader.removeExtension(f)
    val validSpecies = SpeciesLUT(basename)

    validSpecies match {
      case Nil => lines
      case theseSpecies => {
        val filteredLines =
          lines.filter{ case line => theseSpecies.contains(line.split("\t")(1).trim) }
        logger.debug(s"${lines.length - filteredLines.length} entries ignored for $basename")
        filteredLines
      }
    }
  }
}
