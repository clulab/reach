package edu.arizona.sista.bionlp.nxml

import scala.xml._
import scala.xml.factory.XMLLoader
import javax.xml.parsers.SAXParser
import scala.collection.mutable.ListBuffer
import java.io.File
import edu.arizona.sista.bionlp.FriesEntry

// This singleton is necessary to avoid needing the NXML DTD
object MyXML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}

class NxmlReader(removeCitations:Boolean = true, ignoreSections:Seq[String] = Nil) {

  /** Parsed an Nxml document and gives back an NxmlDoc instance */
  def readNxml(doc:Elem, docName:String):Seq[FriesEntry] = {
    var sec_counter = 0
    var fig_counter = 0
    var supm_counter = 0

    def parseSubTree(node:Node, name:String, sectionName:String, sectionId:String):Seq[FriesEntry] = {
      node match {
        case txt:Text => Nil
          Seq(FriesEntry(name, "0", "text", sectionName, false, txt.text))
        case el:Elem =>
         // Figure out how to handle the element node
          el.label match {
            case "p" =>
              // Here we will call recursion but then "reduce" the subsequence
              // to a single FriesEntry
              val components = el.child flatMap (parseSubTree(_, name, sectionName, sectionId))

              // Merge all elements into a single FriesEntry
              var sb = new StringBuilder()
              for(entry <- components){
                sb ++= entry.text
              }

              Seq(FriesEntry(name, "0", sectionId, sectionName, false, sb.toString))
            case "sec" | "fig" | "supplementary-material" =>
              // Figure out how to handle the ids of the nodes
              val (sec_id, norm_id) = el.label match {
                case "sec" =>
                  val lid = el.attribute("id")  match {
                    case Some(id) => s"${id.head}"
                    case None =>
                      sec_counter += 1
                      s"sec-$sec_counter"
                  }
                  // Look for the normalized ids
                  val nid = el.attribute("sec-type") match {
                    case Some(id) => s"${id.head}"
                    case None => lid
                  }

                  (lid, nid)
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
              val text = if (this.removeCitations) " " * el.text.length else el.text
              Seq(FriesEntry(name, "0", "xref", "xref", false, text))
            // The following tags will be ignored
            case "table" | "table-wrap" | "td" =>  Nil
            // Other tags will be treated recursively
            case _ =>  el.child flatMap (parseSubTree(_, name, sectionName, sectionId))
          }
        case _ => Nil
      }
    }

    var entries = new ListBuffer[FriesEntry]

    val front = doc \\ "front"
    val body = doc \\ "body"

    // Get the article title
    val title = front \\ "article-title"

    val titleEntry = if(title.size > 0){
      List(FriesEntry(docName, "0", "article-title", "article-title", true, title.head.text))
    }
    else{
      Nil
    }

    entries ++= parseSubTree(body.head, docName, "body", "body")

    // Get the abstract
    val abs = front \\ "abstract"
    val absEntries = new ListBuffer[FriesEntry]

    if(abs.size > 0){
      val nodes:Seq[FriesEntry] = parseSubTree(abs.head, docName, "abstract", "abstract")
      // Override the generated section names to abstract
      absEntries ++= nodes map (node => FriesEntry(docName, "0", "abstract", "abstract", node.isTitle, node.text))
    }

    val preProcessed = titleEntry ::: absEntries.toList ::: entries.toList

    // Do postprocessing to remove any empty entries and set correcly the chunk id
    val postProcessed = preProcessed filter (e => !e.text.trim.isEmpty)

    postProcessed.zipWithIndex map {
      case (e, ix) =>
        FriesEntry(e.name, s"$ix", e.sectionId, e.sectionName, e.isTitle, e.text)
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
}
