package org.clulab.reach.indexer

import java.io.{FileWriter, PrintWriter, File}
import java.nio.file.Paths

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{TopScoreDocCollector, IndexSearcher}
import org.apache.lucene.store.FSDirectory
import org.slf4j.LoggerFactory

import org.clulab.utils.StringUtils
import NxmlSearcher._


/**
  * Searches the NXML index created by NXML indexer
  * User: mihais
  * Date: 10/19/15
  * Last Modified: Remove unused processors client refs.
  */
class NxmlSearcher(val indexDir:String) {
  val reader = DirectoryReader.open(FSDirectory.open(Paths.get(indexDir)))
  val searcher = new IndexSearcher(reader)

  def close() = reader.close()

  def docs(ids:Set[(Int, Float)]):Set[(Document, Float)] = {
    val ds = new mutable.HashSet[(Document, Float)]()
    for(id <- ids) {
      ds += new Tuple2(searcher.doc(id._1), id._2)
    }
    ds.toSet
  }

  def saveIds(docs:Set[(Document, Float)]): Unit = {
    val os = new PrintWriter(new FileWriter("ids.txt"))
    for(doc <- docs) {
      val id = doc._1.get("id")
      os.println(id)
    }
    os.close()
  }

  def saveNxml(resultDir:String, docs:Set[(Document, Float)], howManyToSave:Int = 0): Unit = {
    val docSeq = if (howManyToSave > 0) {
      docs.toSeq.sortBy(-_._2).take(howManyToSave)
    } else {
      docs.toSeq.sortBy(-_._2)
    }
    val sos = new PrintWriter(new FileWriter(resultDir + File.separator + "scores.tsv"))
    for(doc <- docSeq) {
      val id = doc._1.get("id")
      val nxml = doc._1.get("nxml")
      val os = new PrintWriter(new FileWriter(resultDir + File.separator + id + ".nxml"))
      os.print(nxml)
      os.close()
      sos.println(s"$id\t${doc._2}")
    }
    sos.close()
  }

  def saveDocs(resultDir:String, docIds:Set[(Int, Float)], maxDocs:Int, maxSize:Double = 1E6): Unit = {
    val sos = new PrintWriter(new FileWriter(resultDir + File.separator + "scores.tsv"))
    var count = 0
    for(docId <- docIds if count < maxDocs) {
      val doc = searcher.doc(docId._1)
      val id = doc.get("id")
      val nxml = doc.get("nxml")
      val year = doc.get("year")
      val size = nxml.toString.length * 2 // in bytes
      if(size <= maxSize) {
        val os = new PrintWriter(new FileWriter(resultDir + File.separator + id + ".nxml"))
        os.print(nxml)
        os.close()
        sos.println(s"$id\t${docId._2}\t$year\t$size")
        count += 1
      }
    }
    sos.close()
    logger.info(s"Saved $count documents.")
  }

  def search(query:String, totalHits:Int = TOTAL_HITS):Set[(Int, Float)] = {
    searchByField(query, "text", new StandardAnalyzer(), totalHits)
  }

  def searchId(id:String, totalHits:Int = 1):Set[(Int, Float)] = {
    searchByField(id, "id", new WhitespaceAnalyzer(), totalHits)
  }

  def searchByField(query:String,
                    field:String,
                    analyzer:Analyzer,
                    totalHits:Int = TOTAL_HITS,
                    verbose:Boolean = true):Set[(Int, Float)] = {
    val q = new QueryParser(field, analyzer).parse(query)
    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)
    val hits = collector.topDocs().scoreDocs
    val results = new mutable.HashSet[(Int, Float)]
    for(hit <- hits) {
      val docId = hit.doc
      val score = hit.score
      results += new Tuple2(docId, score)
    }
    if(verbose) logger.debug(s"""Found ${results.size} results for query "$query"""")
    results.toSet
  }

  def intersection(s1:Set[(Int, Float)], s2:Set[(Int, Float)]):Set[(Int, Float)] = {
    val result = new mutable.HashSet[(Int, Float)]()
    for(s <- s1) {
      var found = false
      var otherScore = 0.0.toFloat
      for(o <- s2 if ! found) {
        if(s._1 == o._1) {
          found = true
          otherScore = o._2
        }
      }
      if(found) {
        result += new Tuple2(s._1, s._2 + otherScore)
      }
    }
    result.toSet
  }

  def union(s1:Set[Int], s2:Set[Int]):Set[Int] = {
    val result = new mutable.HashSet[Int]()
    s1.foreach(result += _)
    s2.foreach(result += _)
    result.toSet
  }

  def countDocsContaining(eventDocs:Set[(Int, Float)], token:String):Int = {
    // token could be a phrase; make sure quotes are used
    val query = s"""Ras AND "$token""""
    val result = intersection(eventDocs, search(query))
    result.size
  }

  /** Use case for Natasa, before the Jan 2016 PI meeting */
  def useCase(resultDir:String): Unit = {
    val eventDocs = search("phosphorylation phosphorylates ubiquitination ubiquitinates hydroxylation hydroxylates sumoylation sumoylates glycosylation glycosylates acetylation acetylates farnesylation farnesylates ribosylation ribosylates methylation methylates binding binds")
    val result = intersection(eventDocs, search("""Ras AND (ROS OR "antioxidant response element" OR Warburg OR MAPK OR "Raf/Mek/Erk" OR Akt OR NfkB OR TGFb OR TGFbeta OR TGFb1 OR TGFbeta1 OR integrins OR ADAM OR EGF OR EGFR OR RTK OR apoptosis OR autophagy OR proliferation OR "transcription factors" OR ATM OR p53 OR RB OR "tumor suppressors" OR glycolysis OR "pentose phosphate pathway" OR OXPHOS OR mitochondria OR "cell cycle" OR "energy balance" OR exosomes OR RAGE OR HMGB1)"""))
    logger.debug(s"The result contains ${result.size} documents.")
    val resultDocs = docs(result)
    saveNxml(resultDir, resultDocs, 0)
    saveIds(resultDocs)

    //
    // histogram of term distribution in docs
    //

    logger.debug("Generating topic histogram...")
    val histoPoints = Array(
      "ROS",
      "antioxidant response element",
      "Warburg",
      "MAPK",
      "Raf/Mek/Erk",
      "Akt",
      "NfkB",
      "TGFb",
      "TGFbeta",
      "TGFb1",
      "TGFbeta1",
      "integrins",
      "ADAM",
      "EGF",
      "EGFR",
      "EGFR",
      "RTK",
      "apoptosis",
      "autophagy",
      "proliferation",
      "transcription factors",
      "ATM",
      "p53",
      "RB",
      "tumor suppressors",
      "glycolysis",
      "pentose phosphate pathway",
      "exosomes",
      "OXPHOS",
      "mitochondria",
      "cell cycle",
      "energy balance",
      "RAGE",
      "HMGB1")

    val histoValues = new ArrayBuffer[(String, Int)]()
    for(point <- histoPoints) {
      histoValues += new Tuple2(point, countDocsContaining(result, point))
    }
    val histoFile = new PrintWriter(new FileWriter(resultDir + File.separator + "histo.txt"))
    for(i <- histoValues.sortBy(0 - _._2)) {
      histoFile.println(s"${i._1}\t${i._2}")
    }
    histoFile.close()


    logger.debug("Done.")
  }

  def vanillaUseCase(query:String, resultDir:String, maxDocs:Int = Int.MaxValue) {
    val eventDocs = search(query)
    logger.debug(s"The result contains ${eventDocs.size} documents.")
    saveDocs(resultDir, eventDocs, maxDocs)
    logger.debug("Done.")
  }

  def frailtyUseCase(resultDir:String): Unit = {
    val phrases = Seq(
      "Musculoskeletal Development",
      "Osseointegration",
      "Eye Movement",
      "Biomineralization",
      "Bone Regeneration",
      "Pinch Strength",
      "Motor Activity",
      "Cool-Down Exercise",
      "Dependent Ambulation",
      "Hand Strength",
      "Skeletal Muscle Enlargement",
      "Anaerobic Threshold",
      "Walking",
      "Gait",
      "Standing Position",
      "Physical Fitness",
      "Muscle Stretching Exercises",
      "Pronation",
      "Psychomotor Performance",
      "Bone Density",
      "Posture",
      "Osteolysis",
      "Muscle Fatigue",
      "Osteogenesis",
      "Articular Range of Motion",
      "Core Stability",
      "Postural Balance",
      "Locomotion",
      "Physical Endurance",
      "Exergaming",
      "Isotonic Contraction",
      "Physical Exertion",
      "Musculoskeletal Physiological Phenomena",
      "Excitation Contraction Coupling",
      "Muscle Strength",
      "Muscle Contraction",
      "Intramuscular Absorption",
      "Chondrogenesis",
      "Muscle Development",
      "Cardiorespiratory Fitness",
      "Uterine Contraction",
      "Tonic Immobility Response",
      "Knee-Chest Position",
      "Bone Resorption",
      "Exercise Tolerance",
      "Physiologic Calcification",
      "Stair Climbing",
      "Muscle Tonus",
      "Plyometric Exercise",
      "Isometric Contraction",
      "Walking Speed",
      "Muscle Relaxation",
      "muscle tissue",
      "muscular tissue",
      "fat tissue",
      "adipose tissue",
      "lymphoid tissue",
      "autophagy",
      "nucleophagy",
    )

    val queryStr = phrases map (p => s"(${p})") mkString " OR "

    vanillaUseCase(queryStr, resultDir, 1000000)
  }

  /** Finds all NXML that contain at least one biochemical interaction */
  def useCaseAnyInteraction(resultDir:String, maxDocs:Int) {
    vanillaUseCase(
      "phosphorylation phosphorylates ubiquitination ubiquitinates hydroxylation hydroxylates sumoylation sumoylates glycosylation glycosylates acetylation acetylates farnesylation farnesylates ribosylation ribosylates methylation methylates binding binds activation activates",
      resultDir,
      maxDocs)
  }

  /** Finds all NXML that contain at least one atrial fibrilation term */
  def useCaseAFib(resultDir:String, maxDocs:Int) {
    vanillaUseCase(
      """ "atrial fibrillation" OR afib  """,
      resultDir,
      maxDocs)
  }

  /** Use case for childrens health */
  def useCaseCH(resultDir:String): Unit = {
    vanillaUseCase(
      "children AND ((TNFAlpha AND nutrition) OR (inflammation AND stunting) OR (kcal AND inflammation) OR (protein AND inflammation) OR (nutrition AND inflammation))",
      resultDir)
  }

  def useCaseTB(resultDir:String): Unit = {
    vanillaUseCase(
      """ "chronic inflammation" AND ("tissue damage" OR "tissue repair" OR "wound healing" OR "angiogenesis" OR "fibrosis" OR "resolvin" OR "eicosanoid" OR "tumor-infiltrating lymphocyte" OR "lymphoid aggregate" OR "granuloma" OR "microbiome" OR "short-chain fatty acid") """,
      resultDir)
  }

  //
  // 4 queries for an older use case for Natasa
  //
  // Natasa's use case, first query
  def useCase4a(resultDir:String): Unit = {
    vanillaUseCase("""(TGFbeta1 OR "Transforming Growth Factor beta 1") AND (BMP OR "Bone Morphogenetic Protein")""", resultDir)
  }
  // Natasa's use case, second query
  def useCase4b(resultDir:String): Unit = {
    vanillaUseCase("""(TGFbeta1 OR "Transforming Growth Factor beta 1") AND pancreas""", resultDir)
  }
  // Natasa's use case, third query
  def useCase4c(resultDir:String): Unit = {
    vanillaUseCase("""(BMP OR "Bone Morphogenetic Protein") AND pancreas""", resultDir)
  }
  // Natasa's use case, fourth query
  def useCase4d(resultDir:String): Unit = {
    vanillaUseCase("""(TGFbeta1 OR "Transforming Growth Factor beta 1") AND (BMP OR "Bone Morphogenetic Protein") AND pancreas""", resultDir)
  }

  /** Dengue use case */
  def useCaseDengue(resultDir:String): Unit = {
    vanillaUseCase(
      """(Dengue OR den OR denv OR Dengue-1 Dengue-2 OR Dengue-3 OR Dengue-4 OR Dengue1 Dengue2 OR Dengue3 OR Dengue4 OR Den-1 OR Den-2 OR Den-3 OR Den-4 OR Den1 OR Den2 OR Den3 OR Den4 OR Denv1 OR Denv2 OR Denv3 OR Denv4 Denv-1 OR Denv-2 OR Denv-3 OR Denv-4) AND (serotype OR serotypes OR viremia OR "capillary leakage" OR "hemorrhagic fever" OR "self-limited dengue fever" OR fever OR "dengue shock syndrome" OR "inapparent dengue infection" OR "serial infection" OR "homologous response" OR "heterologous response" OR "immune evasion" OR "arthropod borne" OR mosquito OR mosquitoes OR "mosquito-borne" OR prm OR ns1 OR ns2a OR ns2b OR ns3 OR ns4a OR ns4 OR ns5)""",
      resultDir)
  }

  //
  // Phase III evaluation use cases
  //
  /** Phase III CMU use case (a) */
  def useCasePhase3a(resultDir:String): Unit = {
    // vanillaUseCase(s"""($GAB2) AND ($AKT OR $BETA_CATENIN OR $PAI1 OR $GRB2)""", resultDir)
    vanillaUseCase(s"""($GAB2) AND ($AKT OR $BETA_CATENIN OR $PAI1 OR $GRB2) AND ($PANCREAS)""", resultDir)
  }
  /** Phase III CMU use case (b) */
  def useCasePhase3b(resultDir:String): Unit = {
    //vanillaUseCase(s"""($MEK) AND ($ERK OR $AKT OR $PHASE3_DRUG)""", resultDir)
    //vanillaUseCase(s"""($MEK) AND ($ERK OR $AKT OR $PHASE3_DRUG) AND $PANCREAS""", resultDir)
    //vanillaUseCase(s"""($MEK) AND $PHASE3_DRUG AND ($PANCREAS)""", resultDir) // v1
    vanillaUseCase(s"""($MEK) AND ($PHASE3_DRUG OR $AKT) AND ($PANCREAS)""", resultDir) // v3
  }
  /** Phase III CMU use case (c) 3/1/2017 */
  def useCasePhase3c(resultDir:String): Unit = {
    // SEARCH 1 GAB2
    //vanillaUseCase(s"""($GAB2) AND (phosphatidylinositol OR proliferation OR SHC1 OR PI3K OR PIK3 OR $GRB2 OR PTPN11 OR SFN OR YWHAH OR HCK OR AKT OR $BETA_CATENIN OR Calcineurin OR SERPINE1) NOT "Fc-epsilon receptor" NOT osteoclast NOT "mast cell"""", resultDir)

    // SEARCH 2 catenin
    //vanillaUseCase(s"""$BETA_CATENIN AND (Wnt OR AXIN1 OR AXIN2 OR AXIN OR APC OR CSNK1A1 OR GSK3B OR TCF OR LEF OR TCF\\/LEF OR CDK2 OR PTPN6 OR CCEACAM1 OR insulin OR PML OR RANBP2 OR YAP1 OR GSK3 OR HSPB8 OR SERPINE1 OR AKT OR PTPN13 OR ACAP1 OR MST1R) NOT neuroblasts NOT neurogenesis NOT anoikis NOT cardiac NOT EMT NOT breast NOT embryonic NOT osteoblast NOT synapse NOT muscle NOT renal""", resultDir)

    // SEARCH 3 MEK inh
    //vanillaUseCase(s"""(Pancreas OR PDAC OR "pancreatic cancer") AND ($MEK OR "MEK inhibitor" OR "MEK inhibition" OR Trametinib OR Selumetinib OR Pimasertib OR PD184352 OR PD318088 OR PD0325901 OR AZD6244 OR AZD6300 OR TAK-733) AND ($AKT) AND ($ERK OR Ki67 OR RB)""", resultDir)

    // SEARCH 3 alternative
    vanillaUseCase(s"""(Pancreas OR PDAC OR "pancreatic ductal adenocarcinoma" OR "pancreatic cancer") AND ($MEK OR "MEK inhibitor" OR "MEK inhibition" OR Trametinib OR Selumetinib OR Pimasertib OR PD184352 OR PD318088 OR PD0325901 OR AZD6244 OR AZD6300 OR TAK-733) AND ($AKT OR $ERK OR Ki67 OR RB)""", resultDir)
  }

  /** Phase III May 2017 CMU/UPitt */
  def useCasePhase3d(resultDir:String): Unit = {
    // query partial
    vanillaUseCase(s"""melanoma AND (p70S6K OR S6 OR gsk OR gsk3 OR gsk3a OR gsk3b OR src OR 4ebp1 OR "eIF4E\\-binding protein 1" OR PHASI OR ybi OR YBX1 OR NSEP1 OR YB1 OR CRD OR Y\\-box OR SkMel\\-133)""", resultDir)
  }

  /** Use case for neuro cognitive development */
  def useCaseNCD(resultDir:String): Unit = {
    vanillaUseCase(
      "(children OR fetal OR prenatal OR neonatal OR infant OR childhood) AND (neuro OR cognitive OR early) AND (development OR ECD) AND measure",
      resultDir)
  }
  def useCaseNCD2(resultDir:String): Unit = {
    // changes from original query:
    // removed: "reading", "BRIEF"
    // the 4th AND used to be an OR. I am fairly sure it should be an AND
    vanillaUseCase(
      """(
        |  (children OR fetal OR prenatal OR neonatal OR infant OR childhood OR early) AND
        |  (neuro OR brain OR cognitive) AND
        |  (development OR ECD) AND
        |  (measure OR assess OR assessing OR screen OR screening OR score OR outcome OR quotient)
        |)
        |AND
        |(
        |  (motor OR language OR cognition OR executive function OR social OR emotional OR socio\\-emotional) OR
        |  ("school readiness" OR literacy OR numeracy) OR
        |  (Mullens OR Mullen OR Griffiths OR Wechsler OR WPPSI OR WISC OR WAIS OR Stanford Binet OR Stanford-Binet OR "Peabody Picture Vocabulary Test" OR PPVT OR "Expressive Vocabulary Test" OR EVT OR "Preschool Language Scales" OR PLS OR "Receptive One\\-Word" OR ROWPVT OR "Expressive One\\-Word" OR EOWPVT OR Socio\\-emotional OR "Child Behavior Checklist" OR CBCL OR "Behavior Assessment System for Children" OR BASC OR Connors OR "Strengths and Difficulties Questionnaire" OR SDQ OR "NICHQ Vanderbilt" OR Vanderbilt OR "Behavior Rating Inventory of Executive Function" OR BRIEF OR "Developmental NEuroPSYchological Assessment" OR NEPSY OR Bracken OR BSRA OR BBCS OR "ABC Inventory" OR "Ages and Stages" OR ASQ OR "Extended Ages and Stages Questionnaire" OR EASQ OR "Parents Evaluation of Developmental Status" OR PEDS OR PEDS\\:DM OR Denver OR Beery\\-Buktenica OR "Visual Motor Integration" OR "Beery VMI" OR Bayley OR MacArthur\\-Bates OR MacArthur OR CDI OR MB\\-CDI) OR (EEG OR electroencephalogram OR fNIRS OR "functional near\\-infrared spectroscopy" OR MRI OR "magnetic resonance imaging" OR "eye tracking" OR actigraphy)
        |)
        |(NOT "highway decline aging")
        |""".stripMargin,
      resultDir)
  }

  def useCaseCrop(resultDir:String): Unit = {
    vanillaUseCase(
      """ "crop model" OR "crop modeling" OR "crop modelling" OR "agriculture model" OR "cropping system"  """,
      resultDir)
  }

  def useCaseFall2017Eval(resultDir:String): Unit = {
    vanillaUseCase(""" 
      | AZ628 OR "AZ 628" OR "AZ\-628" OR AZD628 OR
      | B1E OR  
      | "cc\-82" OR
      | MLS006010314 OR
      | GTPL8475 OR
      | SCHEMBL4209241 OR
      | DTXSID70236677 OR
      | "EX\-A302" OR
      | "QCR\-186" OR
      | "MolPort\-023\-293\-542"  OR
      | "ZGBGPEDJXCYQPH\-UHFFFAOYSA\-N" OR
      | C27H25N5O2 OR
      | HMS3265I11 OR
      | HMS3265I12 OR
      | HMS3265J11 OR
      | HMS3265J12 OR
      | HMS3656F21 OR
      | ABP000987 OR
      | BDBM50430022 OR
      | IN2126 OR
      | ZINC38226503 OR
      | AKOS016011304 OR
      | 560S6B5D79 OR
      | BCP9000346 OR
      | "CS\-0091" OR
      | RL05473 OR
      | "NCGC00250380\-01" OR
      | "4CA\-0219" OR
      | "AC\-26863" OR
      | "AJ\-94711" OR
      | AK120786 OR
      | HE069035 OR
      | HE190443 OR
      | "HY\-11004" OR
      | "KB\-47460" OR
      | "SC\-94608" OR
      | SMR004701380 OR
      | AB0008124 OR
      | AX8246202 OR
      | Y0332 OR
      | "W\-5691" OR
      | "J\-510421" OR
      | "BRD\-K05804044\-001\-01\-1"
      | """,
      resultDir)

    vanillaUseCase(
      """ "PLX\-4720" OR PLX4720 OR "PLX 4720" OR
      | AK162149 OR
      | C17H14ClF2N3O3S OR
      | 3c4c OR
      | "Raf Kinase Inhibitor V" OR
      | EQY31RO8HA OR
      | MLS006010065 OR
      | SCHEMBL133733 OR
      | GTPL5703 OR
      | CHEMBL1230020 OR
      | BDBM25617 OR
      | CTK5H0564 OR
      | DTXSID10238711 OR
      | "EX\-A186" OR
      | SYN1069 OR
      | "MolPort\-009\-679\-429" OR
      | HMS3244C03 OR
      | HMS3244C04 OR
      | HMS3244D03 OR
      | HMS3265I09 OR
      | HMS3265I10 OR
      | HMS3265J09 OR
      | HMS3265J10 OR
      | HMS3654M10 OR
      | ACT06829 OR
      | AOB87700 OR
      | ABP000428 OR
      | MFCD14635203 OR
      | RS0085 OR
      | ZINC39059267 OR
      | AKOS015919071 OR
      | "AN\-3700" OR
      | "CS\-0094" OR
      | DB06999 OR
      | "NCGC00187911\-01" OR
      | "NCGC00187911\-02" OR
      | "4CA\-1033" OR
      | "AC\-23171" OR
      | BC660038 OR
      | HE067477 OR
      | HE413624 OR
      | "HY\-51424" OR
      | "KB\-59960" OR
      | "SC\-65663" OR
      | SMR004701225 OR
      | AB0008154 OR
      | "DB\-003736" OR
      | "FT\-0673969" OR
      | ST24048328 OR
      | X7406 OR
      | A19411 OR
      | "Q\-4563" OR
      | "J\-522979" OR
      | "S06\-0032" OR
      | "BRD\-K16478699\-001\-01\-9"
      | """,
      resultDir)

    vanillaUseCase("""
        | SB590885 OR
        | "SB\-590885" OR
        | "J\-501805" OR
        | C27H27N5O2 OR
        | "SB\-590885\-AAD" OR
        | "EX\-A612" OR
        | SYN1077 OR
        | RS0086 OR
        | AKOS024457220 OR
        | AKOS026750409 OR
        | AKOS027288720 OR
        | ZINC142592382 OR
        | "CS\-0093" OR
        | AK257854 OR
        | HE069097 OR
        | HE341635 OR
        | "HY\-10966" OR
        | "SC\-85743" OR
        | Y0277
        | """,
      resultDir) 

    vanillaUseCase("""
        | vemurafenib OR
        | Zelboraf OR
        | PLX4032 OR
        | "PLX\-4032" OR
        | "PLX 4032" OR
        | RG7204 OR
        | "RG 7204" OR
        | "RO 5185426" OR
        | 207SMY3FQT OR
        | "Ro 51\-85426" OR
        | RO5185426 OR
        | NSC761431 OR
        | "RG\-7204" OR
        | "AK\-56796" OR
        | "RO\-5185426" OR
        | C23H18ClF2N3O3S OR
        | vemurafenibum OR
        | "HSDB 8143" OR
        | 3og7 OR
        | GTPL5893 OR
        | "QCR\-44" OR
        | CHEMBL1229517 OR
        | CTK8C3113 OR
        | DTXSID50238710 OR
        | "EX\-A053" OR
        | SYN1161 OR
        | "GPXBXXGIAQBQNI\-UHFFFAOYSA\-N" OR
        | "MolPort\-009\-200\-481" OR
        | "ZX\-AFC000306" OR
        | HMS3265M03 OR
        | HMS3265M04 OR
        | HMS3265N03 OR
        | HMS3265N04 OR
        | HMS3654P09 OR
        | AOB87705 OR
        | "EX\-A1335" OR
        | ABP000429 OR
        | "ANW\-69692" OR
        | BDBM50396483 OR
        | IN2235 OR
        | MFCD18074504 OR
        | ZINC52509366 OR
        | AKOS007930804 OR
        | AM81259 OR
        | "AN\-1313" OR
        | "CS\-0216" OR
        | DB08881 OR
        | "MCULE\-7244406627" OR
        | "ME\-0096" OR
        | "NSC\-761431" OR
        | PB11741 OR
        | RL05788 OR
        | TRA0082406 OR
        | "NCGC00250399\-01" OR
        | "4CA\-1032" OR
        | "AC\-25010" OR
        | BC626573 OR
        | HE070213 OR
        | HE290987 OR
        | HE413595 OR
        | "HY\-12057" OR
        | "KB\-59765" OR
        | "SC\-54548" OR
        | AB0031775 OR
        | AB1009703 OR
        | AX8212389 OR
        | "TC\-158437" OR
        | "FT\-0660388" OR
        | "FT\-0675792" OR
        | ST24048327 OR
        | Y0473 OR
        | A25476 OR
        | D09996 OR
        | "Q\-3409" OR
        | "R\-7204" OR
        | "AB01273970\-01" OR
        | "SR\-01000941568" OR
        | "J\-522975" OR
        | "J\-690009" OR
        | "SR\-01000941568\-1" OR
        | "BRD\-K56343971\-001\-02\-3"
        | """,
      resultDir)

    vanillaUseCase("""
        | selumetinib OR
        | selumetinibum OR
        | "606143\-52\-6" OR
        | AZD6244 OR
        | "AZD 6244" OR
        | "ARRY\-142886" OR
        | "AZD\-6244" OR
        | "ARRY 142886" OR
        | "UNII\-6UH91I579U" OR
        | "ARRY\-886" OR
        | 6UH91I579U OR
        | "NCGC00189073\-01" OR
        | "NCGC00189073\-02" OR
        | "AK\-40782" OR
        | C17H15BrClFN4O3 OR
        | DSSTox_CID_28870 OR
        | DSSTox_RID_83139 OR
        | DSSTox_GSID_48944 OR
        | "Q\-101405" OR
        | "CAS\-606143\-52\-6" OR
        | PubChem21092 OR
        | "JSPY\-st000254" OR
        | SCHEMBL155456 OR
        | GTPL5665 OR
        | "QCR\-91" OR
        | DTXSID3048944 OR
        | CTK8B4577 OR
        | "EX\-A020" OR
        | SYN1016 OR
        | "CYOHGALHFOKKQC\-UHFFFAOYSA\-N" OR
        | "MolPort\-009\-679\-426" OR
        | BCPP000367 OR
        | "CC\-49" OR
        | HMS3244G03 OR
        | HMS3244G04 OR
        | HMS3244H03 OR
        | HMS3265K01 OR
        | HMS3265K02 OR
        | HMS3265L01 OR
        | HMS3265L02 OR
        | HMS3654O03 OR
        | "NSC 741O78" OR
        | AOB87732 OR
        | API01002 OR
        | Tox21_113362 OR
        | ABP000918 OR
        | "ANW\-45526" OR
        | BDBM50355497 OR
        | IN2233 OR
        | MFCD11977472 OR
        | RS0061 OR
        | AKOS015904255 OR
        | Tox21_113362_1 OR
        | "ACN\-031539" OR
        | "AN\-4474" OR
        | BCP9000354 OR
        | "CS\-0059" OR
        | "EX\-8621" OR
        | "NSC\-741078" OR
        | "4CA\-0236" OR
        | "AC\-25059" OR
        | "AJ\-84328" OR
        | AM808016 OR
        | BC004624 OR
        | BC660025 OR
        | "CJ\-17817" OR
        | HE069244 OR
        | HE360517 OR
        | "HY\-50706" OR
        | "SC\-85661" OR
        | AB0007973 OR
        | AX8165426 OR
        | "KB\-135452" OR
        | "TX\-013341" OR
        | "FT\-0674552" OR
        | ST24031611 OR
        | X2640 OR
        | D09666 OR
        | "S\-7764" OR
        | "BRD\-K57080016\-001\-01\-9" OR
        | "I14\-17010" OR
        | 3EW
        | """,
      resultDir)
  }

  def useCaseUPittJan2018(resultDir:String): Unit = {
    vanillaUseCase(
      """
      | ("Transforming growth factor beta" OR TGFbeta OR TGFbeta1 OR "TGF\-beta") AND
      | ("Transforming growth factor beta receptor" OR TGFbetaR OR TGFbeta1R OR TGF\-betaR OR SMAD3 OR SMAD4 OR P27) AND
      | ("Glioblastoma multiforme" OR GBM)
      | """,
      resultDir)
  }

  def searchByIds(ids:Array[String], resultDir:String): Unit = {
    val result = new mutable.HashSet[(Int, Float)]()
    logger.info(s"Searching for ${ids.length} ids: ${ids.mkString(", ")}")
    for(id <- ids) {
      val docs = searchId(id)
      if(docs.isEmpty) {
        logger.info(s"Found 0 results for id $id!")
      } else if(docs.size > 1) {
        logger.info(s"Found ${docs.size} for id $id, which should not happen!")
      } else {
        result ++= docs
      }
    }
    logger.info(s"Found ${result.size} documents for ${ids.length} ids.")
    val resultDocs = docs(result.toSet)

    saveNxml(resultDir, resultDocs)
    saveIds(resultDocs)
  }
}

object NxmlSearcher {
  val logger = LoggerFactory.getLogger(classOf[NxmlSearcher])
  val TOTAL_HITS = 500000

  // necessary for Phase III queries
  // FIXME: removed "ERK1\/2" from ERK, "AKT1\/2" from AKT, "MEK1\/2" from MEK. Too many false positives. Why?
  val ERK = """ERK OR ERK1 OR MK03 OR MAPK3 OR ERK2 OR MK01 OR MAPK1 OR "mitogen\-activated protein kinase 3" OR "mitogen\-activated protein kinase 1""""
  val MEK = """MEK OR MEK1 OR MP2K1 OR MAP2K1 OR MEK2 OR MP2K2 OR MAP2K1 OR "dual specificity mitogen\-activated protein kinase kinase 1" OR "dual specificity mitogen\-activated protein kinase kinase 2""""
  val AKT = """AKT OR AKT1 OR AKT2 OR "rac\-alpha serine\/threonine\-protein kinase" OR "rac-beta serine\/threonine\-protein kinase""""
  val GAB2 = """GAB2 OR "grb2\-associated\-binding protein 2""""
  val BETA_CATENIN = """beta\-catenin OR B\-catenin OR "catenin beta\-1" OR ctnnb1"""
  val PAI1 = """PAI1 OR PAI\-1 OR "PAI 1" OR "plasminogen activator inhibitor 1""""
  val PANCREAS = """pancreas OR pancreatic"""
  val GRB2 = """GRB2 OR "growth factor receptor\-bound protein 2""""
  val PHASE3_DRUG = """AZD6244"""

  def main(args:Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    val indexDir = props.getProperty("index")
    val resultDir = props.getProperty("output")
    val searcher = new NxmlSearcher(indexDir)

    if(props.containsKey("ids")) {
      val ids = readIds(props.getProperty("ids"))
      searcher.searchByIds(ids, resultDir)
    } else {
      searcher.useCaseAnyInteraction(resultDir, 100000)

      //searcher.useCase(resultDir)
      //searcher.useCasePhase3d(resultDir)
      //searcher.useCaseNCD2(resultDir)
      //searcher.useCaseCrop(resultDir)
      //searcher.useCaseFall2017Eval(resultDir)
      //searcher.useCaseUPittJan2018(resultDir)
      //searcher.useCaseAFib(resultDir, 100000)
    }

    searcher.close()
  }

  def readIds(fn:String):Array[String] = {
    val ids = new ArrayBuffer[String]()
    for(line <- Source.fromFile(fn).getLines()) {
      var l = line.trim
      if (! l.startsWith("PMC"))
        l = "PMC" + l
      ids += l
    }
    ids.toArray
  }
}
