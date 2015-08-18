package edu.arizona.sista.bionlp.nxml

import scala.xml._
import scala.collection.mutable.ListBuffer
import java.io.File

class NxmlReader(removeReferences:Boolean = true) {

  /** Parsed an Nxml document and gives back an NxmlDoc instance */
  def readNxml(doc:Elem, docName:Option[String] = None):NxmlDoc = {
    var sec_counter = 0
    var fig_counter = 0
    var supm_counter = 0

    def parseSubTree(node:Node, section:String, norm_section:String):Seq[NxmlEntry] = {
      node match {
        case txt:Text =>Seq(NxmlEntry(0, section, norm_section, false, txt.text))
        case el:Elem =>
         // Figure out how to handle the element node
          el.label match {
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
                    case Some(id) => s"{id.head}"
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
              el.descendant flatMap (parseSubTree(_, sec_id, norm_id))
            // If the element is a title
            case "title" => Seq(NxmlEntry(0, section, norm_section, true, el.text))
            // Ommit the references if specified, but keeping the blank characters
            // TODO: Handle this appropriately. this should be a single fries entry
            case "xref" => if (this.removeReferences) " " * el.text.length else el.text
            // Any other tag type will be ignored
            case _ => Nil
          }
        case _ => Nil
      }
    }

    var entries = new ListBuffer[NxmlEntry]

    val front = doc \\ "front"
    val body = doc \\ "body"

    // Get the article title
    val title = front \\ "article-title"

    if(title.size > 0){
      entries += NxmlEntry(0, "article-title", "article-title", true, title.head.text)
    }

    // Get the abstract
    val abs = front \\ "abstract"

    if(abs.size > 0){
      val nodes:Seq[NxmlEntry] = Nil // TODO: getNodes(abs)
      // Override the generated section names to abstract
      entries ++= nodes map (node => NxmlEntry(node.num, "abstract", "abstract", node.isTitle, node.text))
    }

    NxmlDoc(docName, entries.toList)
  }


  /** Reads an nxml doc from a string */
  def readNxmlString(str:String, docName:Option[String] = None) = this.readNxml(XML.loadString(str), docName)

  /** Reads an nxml file */
  def readNxmlFile(path:String) = {

    // Get the file name sans extenstion as the doc name
    val docName = new File(path).getName.split('.').head

    // Load the file as an XML element and parse it
    this.readNxml(XML.loadFile(path), Some(docName))
  }
}
