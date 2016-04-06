package edu.arizona.sista.reach.nxml

case class FriesEntry(
                       name: String,
                       chunkId: String,
                       sectionId: String,
                       sectionName: String,
                       isTitle: Boolean,
                       text: String,
                       references: Seq[Int] = Seq()) {

  override def toString(): String =  s"$chunkId\t$sectionName\t$sectionId\t${if(isTitle) 1 else 0}\t$text"

}
