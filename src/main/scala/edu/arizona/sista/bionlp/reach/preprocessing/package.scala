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
  type TemplateMap = Map[String, String]

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)
  val preprocessor = new BioNLPProcessor(withRuleNER = false, withCRFNER = false)

  // Retrieve relevant rule labels associated with a kb file basename
  val KBLUT: Map[String, Seq[String]] =
    Map("hmdb" -> Seq("Simple_chemical", "BioChemicalEntity"),
        "chebi" -> Seq("Simple_chemical", "BioChemicalEntity"),
        "ProteinFamilies" -> Seq("Family", "BioChemicalEntity"),
        "tissue-type" -> Seq("Cellular_component", "BioChemicalEntity"),
        "uniprot-proteins" -> Seq("Gene_or_gene_product", "BioChemicalEntity"),
        "uniprot-subcellular-locations" -> Seq("Cellular_component", "BioChemicalEntity"),
        "GO-subcellular-locations" -> Seq("Cellular_component", "BioChemicalEntity"),
        // protein domains
        "InterPro-protein-domains" -> Seq("Site"),
        // biopax (model) files
        "biopax-cellular_component" -> Seq("Cellular_component", "BioChemicalEntity"),
        "biopax-gene_or_gene_product" -> Seq("Gene_or_gene_product", "BioChemicalEntity"),
        "biopax-simple_chemical" -> Seq("Simple_chemical", "BioChemicalEntity"),
        // Manually added synonyms for entities (try to assign real IDs to these)
        "manual-family" -> Seq("Family", "BioChemicalEntity"),
        "manual-cellular_component" -> Seq("Cellular_component", "BioChemicalEntity"),
        "manual-gene_or_gene_product" -> Seq("Gene_or_gene_product", "BioChemicalEntity"),
        "manual-simple_chemical" -> Seq("Simple_chemical", "BioChemicalEntity"),
     // .withDefaultValue(Seq("BioChemicalEntity")),
        "Species" -> Seq("Species"))

  val speciesOfInterest = Seq("Human", "Homo sapiens")

  val SpeciesLUT: Map[String, Seq[String]] =
    Map("uniprot-proteins" -> speciesOfInterest,
        "ProteinFamilies" -> speciesOfInterest).withDefaultValue(Nil)

  val IGNORE_THESE = Seq("chebi", "tissue-type")
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
