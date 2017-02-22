package org.clulab.reach

import org.github.jamm._

import org.clulab.reach.grounding._

/**
  * Test of grounding data structure sizes by reachability.
  *   Last Modified: Show detailed usage.
  */
object ShowMemReachable extends App {

  val meter: MemoryMeter = new MemoryMeter

  println(s"Creating ReachIMKBMentionLookups object...")
  val riml = ReachIMKBMentionLookups

  println(s"Sizing ReachIMKBMentionLookups object...")
  val rimlSize = meter.measureDeep(riml)

  println(s"Sizing individual objects...")
  val cclSize = meter.measureDeep(riml.ContextCellLine)
  val ccl2Size = meter.measureDeep(riml.ContextCellLine2)
  val cctSize = meter.measureDeep(riml.ContextCellType)
  val coSize = meter.measureDeep(riml.ContextOrgan)
  val csSize = meter.measureDeep(riml.ContextSpecies)
  val cttSize = meter.measureDeep(riml.ContextTissueType)

  val sbpSize = meter.measureDeep(riml.StaticBioProcess)
  val sclSize = meter.measureDeep(riml.StaticCellLocation)
  val scl2Size = meter.measureDeep(riml.StaticCellLocation2)
  val scSize = meter.measureDeep(riml.StaticChemical)
  val sdSize = meter.measureDeep(riml.StaticDrug)

  val spSize = meter.measureDeep(riml.StaticProtein)
  val spcSize = meter.measureDeep(riml.StaticProteinComplex)
  val spf0Size = meter.measureDeep(riml.StaticProteinFamily0)
  val spfSize = meter.measureDeep(riml.StaticProteinFamily)
  val spf2Size = meter.measureDeep(riml.StaticProteinFamily2)

  val mgclSize = meter.measureDeep(riml.ModelGendCellLocation)
  val mgcSize = meter.measureDeep(riml.ModelGendChemical)
  val mgpafSize = meter.measureDeep(riml.ModelGendProteinAndFamily)

  val sumSizes = cclSize + ccl2Size + cctSize + coSize + csSize +
                 cttSize + sbpSize + sclSize + scl2Size + scSize +
                 sdSize + spSize + spcSize + spf0Size + spfSize +
                 spf2Size + mgclSize + mgcSize + mgpafSize

  println(s"          ContextCellLine = ${cclSize}")
  println(s"         ContextCellLine2 = ${ccl2Size}")
  println(s"          ContextCellType = ${cctSize}")
  println(s"             ContextOrgan = ${coSize}")
  println(s"           ContextSpecies = ${csSize}")
  println(s"        ContextTissueType = ${cttSize}")
  println(s"         StaticBioProcess = ${sbpSize}")
  println(s"       StaticCellLocation = ${sclSize}")
  println(s"      StaticCellLocation2 = ${scl2Size}")
  println(s"           StaticChemical = ${scSize}")
  println(s"               StaticDrug = ${sdSize}")
  println(s"            StaticProtein = ${spSize}")
  println(s"     StaticProteinComplex = ${spcSize}")
  println(s"     StaticProteinFamily0 = ${spf0Size}")
  println(s"      StaticProteinFamily = ${spfSize}")
  println(s"     StaticProteinFamily2 = ${spf2Size}")
  println(s"    ModelGendCellLocation = ${mgclSize}")
  println(s"        ModelGendChemical = ${mgcSize}")
  println(s"ModelGendProteinAndFamily = ${mgpafSize}")

  println(s"                    TOTAL = ${sumSizes}")
  println(s"                     RIML = ${rimlSize}")
}
