package edu.arizona.sista.assembly

import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.TestUtils._
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.assembly

class TestAssemblyManager extends FlatSpec with Matchers {

  def createDoc(text: String, id: String): Document = {
    val doc = bioproc.annotate(text)
    doc.id = Some(id)
    doc.text = Some(text)
    doc
  }

  val text1 = "Ras is phosphorylated."
  val text2 = "Ras was phosphorylated."
  val text3 = "Ras is phosphorylated at Ser123."
  val text4 = "Ras is phosphorylated by MEK at Ser123."
  val text5 = "Mek binds with Ras."
  val text6 = "Ras binds with MEK."

  val doc1 = createDoc(text1, "assembly-test1")
  val doc2 = createDoc(text2, "assembly-test2")
  val doc3 = createDoc(text3, "assembly-test3")
  val doc4 = createDoc(text4, "assembly-test4")
  val doc5 = createDoc(text5, "assembly-test5")
  val doc6 = createDoc(text6, "assembly-test6")

  val mentions1 = testReach.extractFrom(doc1)
  val mentions2 = testReach.extractFrom(doc2)
  val mentions3 = testReach.extractFrom(doc3)
  val mentions4 = testReach.extractFrom(doc4)
  val mentions5 = testReach.extractFrom(doc5)
  val mentions6 = testReach.extractFrom(doc6)

  s"$text1 $text2 $text3" should "contain a SimpleEntity representation for Ras without a PTM" in {
    val am = AssemblyManager()

    am.trackMentions(mentions1 ++ mentions2 ++ mentions3)

    val ras = mentions1.filter(m => (m matches "Entity") && (m.text.toLowerCase == "ras")).head
    val se = am.getEERepresentation(ras).asInstanceOf[SimpleEntity]
    val ptms = se.modifications.filter{
      case ptm: assembly.PTM => true
      case _ => false
    }

    ptms should have size(0)
  }

  it should "have 3 mentions as evidence" in {
    val am = AssemblyManager()

    am.trackMentions(mentions1 ++ mentions2 ++ mentions3)

    val ras = mentions1.filter(m => (m matches "Entity") && (m.text.toLowerCase == "ras")).head

    val se = am.getEERepresentation(ras)
    val evidence = am.getEvidence(se)

    evidence should have size(3)
  }

  it should "have three SimpleEvent representations for the Phosphorylation of Ras" in {
    val am = AssemblyManager()

    am.trackMentions(mentions1 ++ mentions2 ++ mentions3)

    val phosEvents: Seq[SimpleEvent] =
      am.idToEERepresentation
        .values
        .toSeq
        .filter{
          case se: SimpleEvent => se.label == "Phosphorylation"
          case _ => false
        }
        .map(_.asInstanceOf[SimpleEvent])

    phosEvents should have size(3)
  }

  it should "have two distinct SimpleEvent representations for the Phosphorylation of Ras" in {
    val am = AssemblyManager()

    am.trackMentions(mentions1 ++ mentions2 ++ mentions3)

    val phosEvents: Seq[SimpleEvent] =
      am.idToEERepresentation
        .values
        .toSeq
        .filter{
          case se: SimpleEvent => se.label == "Phosphorylation"
          case _ => false
        }
        .map(_.asInstanceOf[SimpleEvent])

    // FIXME
    phosEvents.groupBy(p => (p.inputHash, p.outputHash, p.label) ) should have size(2)
  }

  text3 should "have a SimpleEntity representing Ras w/ PTM(phos + site) as output to a phosphorylation" in {
    val am = AssemblyManager()

    am.trackMentions(mentions3)

    val p = mentions3.filter(_ matches "Phosphorylation").head

    val phos = am.getEERepresentation(p).asInstanceOf[SimpleEvent]
    val evidence = am.getEvidence(phos)

    evidence should have size(1)

    val output = phos.output
    output should have size(1)

    val outputEntity = output.head.asInstanceOf[SimpleEntity]

    val ptms =
      outputEntity.modifications.filter{
        case ptm: assembly.PTM => true
        case _ => false
      }

    ptms should have size(1)

    val ptm = ptms.head.asInstanceOf[assembly.PTM]

    ptm.label should be("Phosphorylation")

    ptm.site.isDefined should be(true)
  }

  s"$text5 $text6" should "contain 2 Complexes (one for each mention) representing Mek-Ras binding" in {
    val am = AssemblyManager()

    am.trackMentions(mentions5 ++ mentions6)

    val complexes =
      am.idToEERepresentation
        .values
        .filter(_.isInstanceOf[Complex])

    complexes should have size(2)
  }

  it should "have 2 equivalent Complexes representing Mek-Ras binding" in {
    val am = AssemblyManager()

    am.trackMentions(mentions5 ++ mentions6)

    val complexes =
      am.idToEERepresentation
        .values
        .filter(_.isInstanceOf[Complex])
        .map(_.asInstanceOf[Complex])

    val (c1, c2) = (complexes.head, complexes.last)
    c1.isEquivalentTo(c2) should be(true)
  }

  it should "have two mentions as evidence" in {
    val am = AssemblyManager()

    am.trackMentions(mentions5 ++ mentions6)

    val b = mentions5.filter(_ matches "Binding").head

    val complex = am.getEERepresentation(b)
    val evidence = am.getEvidence(complex)

    evidence should have size(2)
  }

}
