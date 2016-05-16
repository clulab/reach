package edu.arizona.sista.reach.nxml

import scala.xml._
import scala.xml.factory.XMLLoader
import javax.xml.parsers.{SAXParser,SAXParserFactory}
import scala.xml.transform.RewriteRule
import scala.collection.mutable.ListBuffer
import java.io.File
import scala.collection.mutable

// This singleton is necessary to avoid loading NXML's DTD
object MyXML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}
//////////////////////////////////////////////////////////

object PreprocessNxml extends RewriteRule {
  // surrounds sup/sub content with spaces
  override def transform(n: Node): Seq[Node] = n match {
    case <sup>{text}</sup> => <sup> {text} </sup>
    case <sub>{text}</sub> => <sub> {text} </sub>
    case elem: Elem => elem.copy(child = elem.child.flatMap(this.transform))
    case other => other
  }
}

//////////////////////////////////////////////////////////

class NxmlReader(ignoreSections:Seq[String] = Nil) {

  // This is a regex to remove the artifacts left from citation removal
  val citationArtifact = """[\(\[][ ,;(and)]+[\)\]]""".r

  /** Parsed an Nxml document and gives back an NxmlDoc instance */
  def readNxml(originalDoc:Elem, docName:String):Seq[FriesEntry] = {
    var sec_counter = 0
    var fig_counter = 0
    var supm_counter = 0

    val doc = PreprocessNxml(originalDoc)

    def parseSubTree(node:Node, name:String, sectionName:String, sectionId:String):Seq[FriesEntry] = {
      node match {
        case txt:Text => Nil
          Seq(FriesEntry(name, "0", sectionId, sectionName, false, txt.text))
        case el:Elem =>
         // Figure out how to handle the element node
          el.label match {
            case "p" =>
              // Here we will call recursion but then "reduce" the subsequence
              // to a single FriesEntry
              val components = el.child filter (_.label != "fig") flatMap (parseSubTree(_, name, sectionName, sectionId))

              // Merge all elements into a single FriesEntry
              var sb = new StringBuilder()
              val references = new mutable.ArrayBuffer[Int]
              var position = 0
              for(entry <- components){
                if(entry.sectionName != "xref"){
                  sb ++= entry.text
                  position += entry.text.length
                }
                else
                  references += position
              }

              val textEntry = Seq(FriesEntry(name, "0", sectionId, sectionName, false, sb.toString, references.toSeq))

              val figs = el.child filter (_.label == "fig") flatMap(parseSubTree(_, name, sectionName, sectionId))

              figs ++ textEntry

            case "sec" | "fig" | "supplementary-material" =>

              // Otherwise, figure them out
              // Figure out how to handle the ids of the nodes
              val (sec_id, norm_id) = el.label match {
                case "sec" =>
                // If the id and name of the section are provided, use them
                  if (sectionId != "" && sectionName != ""){
                    (sectionName, sectionId)
                  }
                  else{
                    val lid = el.attribute("id")  match {
                      case Some(id) => s"${id.head}"
                      case None =>
                        sec_counter += 1
                        s"sec-$sec_counter"
                    }
                    // Look for the normalized ids
                    val nid = el.attribute("sec-type") match {
                      case Some(id) => s"${id.head}"
                      case None => "N/A"
                    }

                    (lid, nid)
                  }
                case "fig" =>
                  fig_counter += 1
                  val lid = s"fig-$fig_counter"
                  (lid, lid)
                case "supplementary-material" =>
                  supm_counter += 1
                  val lid = s"supm-$supm_counter"
                  (lid, lid)
              }
              // Recursively return the sequence of NxmlEntries for the descendants
              el.child flatMap (parseSubTree(_, name, sec_id, norm_id))
            // If the element is a title
            case "title" | "label" => Seq(FriesEntry(name, "0", sectionId, sectionName, true, el.text))
            // Ommit the references if specified, but keeping the blank characters
            case "xref" =>
              val text = " " * el.text.length
              val e = FriesEntry(name, "0", "xref", "xref", false, text)
              Seq(e)
            // The following tags will be ignored
            case "table" | "table-wrap" | "td" | "ack" | "glossary" | "ref-list" | "fn-group" =>  Nil
            // Other tags will be treated recursively
            case _ =>  el.child flatMap (parseSubTree(_, name, sectionName, sectionId))
          }
        case _ => Nil
      }
    }

    val bodyEntries = new ListBuffer[FriesEntry]
    val backEntries = new ListBuffer[FriesEntry]
    val floatsEntries = new ListBuffer[FriesEntry]

    val front = doc \\ "front"
    val body = doc \\ "body"
    val back = doc \\ "back"
    val floats = doc \\"floats-group"

    // Get the article title
    val title = front \\ "article-title"
    val titleEntry = if (title.size > 0)
        List(FriesEntry(docName, "0", "article-title", "article-title", true, title.head.text))
      else Nil

    val metaData = getPublicationMetaData(docName, front)

    bodyEntries ++= body flatMap (parseSubTree(_, docName, "", ""))

    backEntries ++= back flatMap (parseSubTree(_, docName, "", ""))

    floatsEntries ++= floats flatMap (parseSubTree(_, docName, "", ""))

    // Get the abstract
    val abs = front \\ "abstract"
    val absEntries = new ListBuffer[FriesEntry]

    if(abs.size > 0){
      for(a <- abs){
        val nodes:Seq[FriesEntry] = parseSubTree(a, docName, "abstract", "abstract")
        // Override the generated section names to abstract
        absEntries ++= nodes map (node => FriesEntry(docName, "0", "abstract", "abstract", node.isTitle, node.text.replace('\n', ' ')))
      }
    }

    val preProcessed = titleEntry ::: metaData.toList ::: absEntries.toList ::: bodyEntries.toList ::: backEntries.toList ::: floatsEntries.toList

    // Do postprocessing to remove any empty entries and set correcly the chunk id
    val postProcessed = preProcessed filter (e => e.sectionName == "xref" || !e.text.trim.isEmpty)

    // Do some more postprocessing before returning
    postProcessed.zipWithIndex map {
      // Assign the chunkId correctly
      case (e, ix) =>
        FriesEntry(e.name, s"$ix", e.sectionId, e.sectionName, e.isTitle, e.text.replace('\n', ' '), e.references)
    } filter {
      // Remove the entries in the ignoreSections list
      entry => !(this.ignoreSections contains entry.sectionId)
    } map {
      // Remove artifacts of citation removal
      e => FriesEntry(e.name, e.chunkId, e.sectionId,
         e.sectionName, e.isTitle,
        citationArtifact.replaceAllIn(e.text, ""), e.references)
    } map {
      // Remove unwanted new lines and tabs
      e => FriesEntry(e.name, e.chunkId, e.sectionId,
        e.sectionName, e.isTitle,
        e.text.replace('\n', ' ').replace('\t', ' '), e.references)
    }
  }

  /** Reads an nxml doc from a string */
  def readNxml(str:String, docName:String):Seq[FriesEntry] = this.readNxml(MyXML.loadString(str), docName)

  /** Reads an nxml file */
  def readNxml(path:String):Seq[FriesEntry] = {

    // Get the file name sans extenstion as the doc name
    val docName = new File(path).getName.split('.').head

    // Load the file as an XML element and parse it
    this.readNxml(MyXML.loadFile(path), docName)
  }

  def readNxml(file:File):Seq[FriesEntry] = this.readNxml(file.getPath)


  /** Create sections for metadata related to a publication. */
  private def getPublicationMetaData (docName:String, front: Seq[Node]): ListBuffer[FriesEntry] = {
    val metaData = new ListBuffer[FriesEntry]

    val pubYear = front \\ "pub-date" \ "year"
    if (pubYear.size > 0)
      metaData.append(FriesEntry(docName, "0", "meta-data", "publication-year",
                                 true, pubYear.head.text))

    val pubMonth = front \\ "pub-date" \ "month"
    if (pubMonth.size > 0)
      metaData.append(FriesEntry(docName, "0", "meta-data", "publication-month",
                                 true, pubMonth.head.text))
    metaData
  }

}
