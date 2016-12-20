package org.clulab.reach.export.cmu

import org.clulab.odin.Mention
import org.clulab.reach.assembly.export.{ AssemblyExporter, AssemblyRow }
import org.clulab.reach.assembly.representations.EntityEventRepresentation
import org.clulab.reach.mentions._

import scala.collection.mutable.ListBuffer


class CMURow(
  input: String,
  output: String,
  source: String, // only for Translocation
  destination: String, // only for Translocation
  controller: String,
  val nestedControllers: (List[String], List[String]), // _1 = positive controllers, _2 = negative controllers
  eerID: String,
  label: String,
  val mechanismType: String, // what simple event is regulated by this regulation
  precededBy: Set[String],
  negated: Boolean,
  evidence: Set[Mention],
  // to make debugging easier
  eer: EntityEventRepresentation
) extends AssemblyRow(
  input,
  output,
  source,
  destination,
  controller,
  eerID,
  label,
  precededBy,
  negated,
  evidence,
  eer
) {
  /**
    * Translates Reach namespaces into element types
    * Note that this only worries about entities that can serve as elements in the CMU format
    **/
  private def elementType(input:String):String = {
    val db = elementDatabase(input)

    if (db.startsWith("{")) {
      val dbs = db.substring(1, db.length - 1).split(""",\s*""")
      val b = new StringBuilder
      b.append("{")
      var first = true
      for(d <- dbs) {
        if(! first) b.append(", ")
        b.append(singleElementType(d))
        first = false
      }
      b.append("}")
      b.toString()
    } else {
      singleElementType(db)
    }
  }

  private def singleElementType(db:String):String = {
    val t = db match {
      case "uniprot" => "Protein"
      case "pfam" => "Protein Family"
      case "interpro" => "Protein Family"
      case "be" => "Protein Family|Protein Complex"
      case "pubchem" => "Chemical"
      case "hmdb" => "Chemical"
      case "chebi" => "Chemical"
      case "go" => "Biological Process"
      case "mesh" => "Biological Process"
      case _ => "Other"
    }
    //println(s"TYPE from $db is $t")
    t
  }

  private def elementName(input:String):String = {
    val v = elementParser(input)
    if(v.isDefined) v.get._1
    else AssemblyExporter.NONE
  }

  private def elementDatabase(input:String):String = {
    elementParser(input) match {
      case v: Some[(String, String, String)] => v.get._2
      case _ => AssemblyExporter.NONE
    }
  }

  private def elementIdentifier(input:String):String = {
    val v = elementParser(input)
    if(v.isDefined) v.get._3
    else AssemblyExporter.NONE
  }

  private def elementParser(input:String):Option[(String, String, String)] = {
    val components = tokenizeComponents(input)

    if (components.length == 1)
      return parseSingleElement(components(0))

    // handle complexes below
    val parsedComponents = components.map(parseSingleElement)

    val names = new ListBuffer[String]
    val dbs = new ListBuffer[String]
    val ids = new ListBuffer[String]
    for(c <- parsedComponents) {
      if(c.isDefined) {
        names += c.get._1
        dbs += c.get._2
        ids += c.get._3
      }
    }

    if(ids.isEmpty) return None
    Some(mkComplexPart(names), mkComplexPart(dbs), mkComplexPart(ids))
  }

  private def mkComplexPart(components:Seq[String]):String =
    s"""{${components.mkString(", ")}}"""

  def parseSingleElement(input:String): Option[(String, String, String)] = {
    val m = CMUExporter.ELEMENT_PATTERN.matcher(input)
    if(m.matches()) {
      val name = m.group(1)
      val db = m.group(2).toLowerCase()
      val id = m.group(3)
      return Some(name, db, id)
    }
    None
  }

  private def tokenizeComponents(input:String):Array[String] = {
    if(input.startsWith("{") && input.endsWith("}")) {
      // found a complex; break it down into its components
      val components = input.substring(1, input.length - 1).split(""",\s*""").map(_.trim)
      components
    } else {
      // just a single entity
      Array(input)
    }
  }

  private def indirectLabel(isIndirect:Boolean): String =
    if(isIndirect) "I" else "D"

  private def removePTM(elem:String):String = {
    val dotOffset = elem.lastIndexOf('.')
    if(dotOffset > 0) {
      val e = elem.substring(0, dotOffset)
      // println(s"AFTER PTM: $e")
      e
    } else {
      elem
    }
  }

  /** Remove namespace from location ids, which are sufficient */
  private def removeNamespaceFromId(id:String): String = {
    val colonOffset = id.indexOf(':')
    if(colonOffset > 0) id.substring(colonOffset + 1).toLowerCase
    else id.toLowerCase
  }

  private def getPositiveControllerNames: String = {
    //println(s"POSITIVE CONTROLLERS = ${nestedControllers._1.mkString(", ")}")
    val b = new StringBuilder
    val names = nestedControllers._1.map(elementName)
    var first = true
    for(n <- names) {
      if(! first)
        b.append(", ")
      b.append(n)
      if(names.size > 1)
        b.append("*1")
      if(! first)
        b.append("+")
      first = false
    }
    b.toString()
  }
  private def getPositiveControllerTypes: String = {
    val b = new StringBuilder
    val types = nestedControllers._1.map(elementType)
    b.append(types.mkString(", "))
    b.toString()
  }
  private def getPositiveControllerIds: String = {
    val b = new StringBuilder
    val ids = nestedControllers._1.map(elementIdentifier)
    b.append(ids.mkString(", "))
    b.toString()
  }

  private def getNegativeControllerNames: String = {
    val b = new StringBuilder
    val names = nestedControllers._2.map(elementName)
    var first = true
    for(n <- names) {
      if(! first)
        b.append(", ")
      b.append(n)
      if(names.size > 1)
        b.append("*1")
      if(! first)
        b.append("-")
      first = false
    }
    b.toString()
  }
  private def getNegativeControllerTypes: String = {
    val b = new StringBuilder
    val types = nestedControllers._2.map(elementType)
    b.append(types.mkString(", "))
    b.toString()
  }
  private def getNegativeControllerIds: String = {
    val b = new StringBuilder
    val ids = nestedControllers._2.map(elementIdentifier)
    b.append(ids.mkString(", "))
    b.toString()
  }

  private def getLocation:String = {
    val id = if(label != "Translocation")
      contextFromEvidence(AssemblyExporter.CELLULAR_COMPONENT)
    else
      destination
    removeNamespaceFromId(id)
  }

  private def getControllerLocation:String = {
    val id = if(label != "Translocation")
    // same as getLocation
      contextFromEvidence(AssemblyExporter.CELLULAR_COMPONENT)
    else
      source
    removeNamespaceFromId(id)
  }

  private def locationName(id:String):String = {
    if(id.trim.isEmpty) return ""
    CMUExporter.CMU_KNOWN_LOCATIONS.getOrElse(id, "Other")
  }

  private def getPositiveControllerLocationName: String = {
    if(nestedControllers._1.nonEmpty)
      cleanText(locationName(getControllerLocation))
    else
      ""
  }
  private def getPositiveControllerLocationId: String = {
    if(nestedControllers._1.nonEmpty)
      getControllerLocation
    else
      ""
  }
  private def getNegativeControllerLocationName: String = {
    if(nestedControllers._2.nonEmpty)
      cleanText(locationName(getControllerLocation))
    else
      ""
  }
  private def getNegativeControllerLocationId: String = {
    if(nestedControllers._2.nonEmpty)
      getControllerLocation
    else
      ""
  }

  private def setToString(s: Set[String]): String = s.toSeq.sorted.mkString(", ")

  override val columns: Map[String, String] = {
    baseColumns ++
    Map(
      // operations specific to the CMU tabular format
      CMUExporter.CMU_ELEMENT_NAME -> cleanText(elementName(removePTM(output))),
      CMUExporter.CMU_ELEMENT_TYPE -> cleanText(elementType(removePTM(output))),
      CMUExporter.CMU_DATABASE_NAME -> cleanText(elementDatabase(removePTM(output))),
      CMUExporter.CMU_ELEMENT_IDENTIFIER -> cleanText(elementIdentifier(removePTM(output))),
      CMUExporter.CMU_LOCATION -> cleanText(locationName(getLocation)),
      CMUExporter.CMU_LOCATION_IDENTIFIER -> getLocation,
      CMUExporter.CMU_CELL_LINE -> contextFromEvidence(AssemblyExporter.CELL_LINE),
      CMUExporter.CMU_CELL_TYPE -> contextFromEvidence(AssemblyExporter.CELL_TYPE),
      CMUExporter.CMU_ORGANISM -> contextFromEvidence(AssemblyExporter.ORGAN),
      CMUExporter.CMU_POS_REG_NAME -> getPositiveControllerNames,
      CMUExporter.CMU_POS_REG_TYPE -> getPositiveControllerTypes,
      CMUExporter.CMU_POS_REG_ID -> getPositiveControllerIds,
      CMUExporter.CMU_POS_REG_LOCATION -> getPositiveControllerLocationName,
      CMUExporter.CMU_POS_REG_LOCATION_ID -> getPositiveControllerLocationId,
      CMUExporter.CMU_NEG_REG_NAME -> getNegativeControllerNames,
      CMUExporter.CMU_NEG_REG_TYPE -> getNegativeControllerTypes,
      CMUExporter.CMU_NEG_REG_ID -> getNegativeControllerIds,
      CMUExporter.CMU_NEG_REG_LOCATION -> getNegativeControllerLocationName,
      CMUExporter.CMU_NEG_REG_LOCATION_ID -> getNegativeControllerLocationId,
      CMUExporter.CMU_IS_INDIRECT -> indirectLabel(isIndirect),
      CMUExporter.CMU_MECHANISM_TYPE -> mechanismType,
      CMUExporter.CMU_PAPER_ID -> setToString(docIDs),
      CMUExporter.CMU_EVIDENCE  -> getTextualEvidence.mkString(AssemblyExporter.CONCAT)
    )
  }
}

object CMURow {

  def apply(
    input: String,
    output: String,
    source: String, // only for Translocation
    destination: String, // only for Translocation
    controller: String,
    nestedControllers: (List[String], List[String]), // _1 = positive controllers, _2 = negative controllers
    eerID: String,
    label: String,
    mechanismType: String, // what simple event is regulated by this regulation
    precededBy: Set[String],
    negated: Boolean,
    evidence: Set[Mention],
    // to make debugging easier
    eer: EntityEventRepresentation
  ) = new CMURow(
    input,
    output,
    source,
    destination,
    controller,
    nestedControllers,
    eerID,
    label,
    mechanismType,
    precededBy,
    negated,
    evidence,
    eer
  )
}