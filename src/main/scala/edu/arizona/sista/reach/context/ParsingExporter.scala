package edu.arizona.sista.reach.context

import java.io.File
import io.Source
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.discourse.rstparser.DiscourseTree
import edu.arizona.sista.struct.{Tree, DirectedGraph, DirectedGraphEdgeIterator}
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import scala.collection.JavaConverters._

/***
 *  Exports:
 *      Dependency parse per sentence
 *      Constituency parse per sentence
 *      RST parse per document

 *  For use with NetworkX in Python
 */
object ParsingExporter extends App {
    // use specified config file or the default one if one is not provided

    val sentencesFile = new File(args(0))
    val sectionsFile = new File(args(1))
    val titlesFile = new File(args(2))

    println(s"Parsing files: $sentencesFile, $sectionsFile ...")

    val sentencesLines = Source.fromFile(sentencesFile).getLines.toList
    val sectionsLines = Source.fromFile(sectionsFile).getLines.toList
    val titlesLines = Source.fromFile(titlesFile).getLines.toList

    // Filter out the sections
    val sentences = sentencesLines zip sectionsLines filter { case (sen, sec) => !sec.startsWith("fig") } map (_._1)
    val titles = titlesLines zip sectionsLines filter { case (titl, sec) => !sec.startsWith("fig") } map (_._1)
    val sections = sectionsLines filter { sec => !sec.startsWith("fig") }

    // Annotate
    val proc = new BioNLPProcessor(withDiscourse=true)
    val doc = proc annotateFromSentences (sentences)

    // Fetch the dependency parses - Use collapsed dependencies
    val deps =  doc.sentences.zipWithIndex flatMap {
        case (s, i) =>
            s.dependencies match {
                case Some(dependencies) =>
                    val edges = new DirectedGraphEdgeIterator(s.dependencies.get)
                    val graph = dependencyStrings(edges)
                    graph map { x => s"${i}\t${x}"}
                case None => s"$i\t"
            }

    }

    // Fetch the RST parse
    val rstTrees = createDiscourseTrees(sentences, titles, sections, proc)

    val disc = rstTrees map {
        case (s, e, t) => s"$s\t$e\t${discourseString(t)}"
    }

    // Fetch POS tags
    val posTags = doc.sentences.zipWithIndex map {
        case (s, i) =>
            s.tags match {
                case Some(tags) => s"$i\t${tags.mkString(" ")}"
                case None => s"$i\t"
            }

    }

    // Write down to files
    FileUtils.writeLines(new File("deps.txt"), deps.toList.asJavaCollection)
    FileUtils.writeLines(new File("pos.txt"), posTags.toList.asJavaCollection)
    FileUtils.writeLines(new File("disc.txt"), disc.toList.asJavaCollection)

    // Returns triples (Starting sentence offset, finishing sentence offset +1, Discourse tree)
    def createDiscourseTrees(sentences:Seq[String], titles:Seq[String]
        , sections:Seq[String], proc:BioNLPProcessor):Seq[(Int, Int, DiscourseTree)] = {
        val sens:Seq[(String, Int)] = sentences.zipWithIndex

        val secs:Map[String, Seq[(String, Int)]] = sens.groupBy(s => sections(s._2))

        secs.map{
            case (k, v) =>
                // k: section - v: Seq((line, ix)) tuple
                val doc = proc.annotateFromSentences(v.map(_._1))
                (v(0)._2, v.takeRight(1).apply(0)._2+1, doc.discourseTree.get)
        }.toSeq
    }
    def dependencyStrings(d:Iterator[(Int, Int, String)]) = d.map{ e => s"${e._1} ${e._2} {'label':'${e._3}'}" }.toList
    def discourseString(d:DiscourseTree):String = {
        val direction = d.relationDirection.toString

        if(d.isTerminal){
            val text = d.rawText.replace("'", "\\'")
            s"{'start':(${d.firstToken.sentence}, ${d.firstToken.token}), 'end':(${d.lastToken.sentence}, ${d.lastToken.token}), 'text':'$text'}"
        }
        else{
            val left = discourseString(d.children(0))
            val right = discourseString(d.children(1))
            s"{'label':'${d.relationLabel}', 'direction':'$direction', 'children':[$left, $right]}"
        }
    }
}
