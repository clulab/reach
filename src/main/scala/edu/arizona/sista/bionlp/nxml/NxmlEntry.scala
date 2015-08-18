/***
 * Represents an Nxml2Fries Entry from the legacy tsv files.
 * An entry is usually a paragraph of the document, but could be an image caption
 * or the title of a section.
 *
 * Written by: Enrique Noriega <enoriega@email.arizona.edu> on 08/17/15
 **/

package edu.arizona.sista.bionlp.nxml

import scala.collection.mutable.ListBuffer

case class NxmlEntry(num:Int, section:String, normalizedSection:String,
    isTitle:Boolean, text:String){

      /***
       * Generates a string ready to be printed as if it came from the
       * legacy nxml2fries
       **/
      override def toString(): String =   Seq(num, section, normalizedSection,
      if(isTitle) 1 else 0, text).mkString("\t")

    }

case class NxmlDoc(name:Option[String], entries:Seq[NxmlEntry]) {

  override def toString(): String = this.toString(true)

  def toString(printHeader:Boolean): String = {
    var str = new ListBuffer[String]

    if(printHeader){
      str += "chunk_id\tsection_id\tname\tis_title\ttext"
    }

    entries foreach ( str +=  _.toString )
    str.toList.mkString("\n")
  }

}
