package org.clulab.reach
import org.clulab.reach.mentions._
import TestUtils._
import org.clulab.odin.{EventMention, Mention, RelationMention, TextBoundMention}
import org.scalatest.{FlatSpec, Matchers}


class Debug_PolarityEngine extends FlatSpec with Matchers{
  val CONTROLLER = "__controller__"
  val CONTROLLED = "__controlled__"
  val THEME = "theme"

  def showSenProcessingResult(sent:String): Unit ={
    println("=========================")
    println("sentence:", sent)
    val mentions = getBioMentions(sent, false)
    for (mention <-mentions){

      if (mention matches "ComplexEvent") {
        if (!mention.isInstanceOf[CorefRelationMention]) {
          var lemmas = mention.sentenceObj.words.clone()
          val lemmas_masked = maskEvent(lemmas, mention.asInstanceOf[BioEventMention], "tag")
          println("\t--------------------------------")
          println("\tevent:", mention.text)
          println("\texpanded masked event:", lemmas_masked.toSeq.mkString(" "))
        }
      }
    }
  }

  def maskEvent(lemmas_raw:Array[String], event:BioMention,  maskOption:String): Array[String] ={
    // filter out the edge cases where event has no controller or controlled.
    var lemmas = lemmas_raw.clone()
    if (event.arguments.contains(CONTROLLER) && event.arguments.contains(CONTROLLED)){
      // filter out the edge cases where controller or controlled is vector.
      //if (event.arguments(CONTROLLER).isInstanceOf[mutable.ArraySeq[Mention]] && event.arguments(CONTROLLED).isInstanceOf[mutable.ArraySeq[Mention]]) {
      if (event.arguments(CONTROLLER).isInstanceOf[Seq[Mention]] && event.arguments(CONTROLLED).isInstanceOf[Seq[Mention]]) {
        // recursively masking the controller and controlled
        val controller = event.arguments(CONTROLLER).head
        lemmas = controller match {
          case controller:RelationMention => {
            maskRecursively(lemmas, controller, maskOption,CONTROLLER)}
          case controller:EventMention => {
            maskRecursively(lemmas, controller, maskOption,CONTROLLER)}
          case controller:TextBoundMention => {
            maskDirect(lemmas, maskOption, CONTROLLER, controller.start, controller.end)}
          case _ => lemmas
        }
        val controlled = event.arguments(CONTROLLED).head
        lemmas = controlled match {
          case controlled:RelationMention => {
            maskRecursively(lemmas, controlled,maskOption, CONTROLLED)}
          case controlled:EventMention => {
            maskRecursively(lemmas, controlled,maskOption, CONTROLLED)}
          case controlled:TextBoundMention => {
            maskDirect(lemmas, maskOption, CONTROLLED, controlled.start, controlled.end)}
          case _ => lemmas
        }
        val (start, end) = getExpandBound(event, controller.start, controlled.start)

        lemmas.slice(start, end)

      } else lemmas_raw.slice(event.start, event.end)

    } else lemmas_raw.slice(event.start, event.end)

  }
  // recursively mask the event
  def maskRecursively(lemmas:Array[String], mention:Mention,  maskOption:String, role:String):Array[String] = {
    if (mention.arguments.contains(THEME)){
      maskDirect(lemmas, maskOption, role, mention.arguments(THEME).head.start, mention.arguments(THEME).head.end)
    }
    else{
      lemmas
    }
  }


  def maskDirect(lemmas:Array[String], maskOption:String, role:String, intervalStart:Int, intervalEnd:Int) : Array[String]= {
    if (role==CONTROLLER){
      if (maskOption == "tag_name") {
        for (index <- intervalStart until intervalEnd) {
          lemmas(index) = "controller_" + lemmas(index)
        }
      }
      else if (maskOption == "tag") {
        for (index <- intervalStart until intervalEnd) {
          if (lemmas(index).toLowerCase.endsWith("kd")) {
            lemmas(index) = "__controller__-kd"
          }
          else {
            lemmas(index) = "__controller__"
          }
        }
      }
    }
    if (role==CONTROLLED){
      if (maskOption == "tag_name") {
        for (index <- intervalStart until intervalEnd) {
          lemmas(index) = "controlled_" + lemmas(index)
        }
      }
      else if (maskOption == "tag") {
        for (index <- intervalStart until intervalEnd) {
          if (lemmas(index).toLowerCase.endsWith("kd")) {
            lemmas(index) = "__controlled__-kd"
          }
          else {
            lemmas(index) = "__controlled__"
          }
        }
      }
    }
    lemmas
  }

  def getExpandBound(event:BioMention, controller_start:Int, controlled_start:Int):(Int, Int) = {
    val event_start = event.start
    val event_end = event.end

    var event_start_new = event_start
    var event_end_new = event_end

    val dependencyTreeObj = event.document.sentences(0).dependencies.get.allEdges

    for (edge <- dependencyTreeObj){
      val potentialBound = Seq(edge._1, edge._2)
      if (potentialBound.contains(controller_start)|| potentialBound.contains(controlled_start)){
        event_start_new = math.min(potentialBound.min, event_start_new)
        event_end_new = math.max(potentialBound.max, event_end_new)
      }
    }
    (event_start_new, event_end_new)
  }


  val failed_sentences = List(
  "The inhibition of ASPP1 increases the phosphorylation of ASPP2.",
  "Note that only K650M and K650E-FGFR3 mutants cause STAT1 phosphorylation",
  "Taken together , these data suggest that decreased PTPN13 expression enhances EphrinB1 and Erk1 and phosphorylation in epithelial cells .",
  "These data are consistent with EphrinB1 being a PTPN13 phosphatase substrate and suggest that decreased PTPN13 expression in BL breast cancer cell lines increases phosphorylation of EphrinB1 .",
  "Cells expressing ErbB3 show tyrosine phosphorylation in response to RAS inhibition",
  "The phosphorylation of AFT by BEF is inhibited by the ubiquitination of Akt.",
  "The phosphorylation of AKT1 following MEK activation.",
  "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D), and accordingly, MEK inhibition substantially increased tyrosine phosphorylated ERBB3 levels (Figure 1A).",
  "ATP reduced GSH depletion",
  "ATP can deplete GSH in cells",
  "ATP depletes GSH rapidly in cells",
  "Nucleotide free Ras inhibits PI3KC2Beta activity.",
  "Of these, 6 involved wortmannin or LY-294002 (inhibitors of phosphoinositide 3-kinase (PI3K)) or rapamycin (an inhibitor of the mammalian target of rapamycin complex 1 (mTORC1)).",
  "Insulin inhibits adipocyte hormone sensitive lipase and activates lipoprotein lipase [XREF_BIBR, XREF_BIBR].",
  "Unexpectedly co-misexpression of Trbl and Akt in the fat body led to a significant reduction in total FoxO levels (XREF_FIG), suggesting that Trbl and Akt might act combinatorially to direct FoxO turnover.",
  "Negatively regulating IRFs include IRF4 that competitively inhibits IRF5 from binding to TLR, thereby inhibiting inflammatory responses.")

  for (sent<-failed_sentences){
    showSenProcessingResult(sent)
  }
}


