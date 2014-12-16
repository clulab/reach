package edu.arizona.sista.bionlp.reach.indexer

import java.io.{PrintStream, FileOutputStream, FilenameFilter, File}

import edu.arizona.sista.utils.StringUtils._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Extracts from a OpenAccess dump the ~1K documents part of the BigMechanism corpus
 * User: mihais
 * Date: 11/10/14
 */
object MakeBigMechCorpus {
  def loadMappings(idFile:String):Map[String, String] = {
    val h = new mutable.HashMap[String, String]
    for(l <- io.Source.fromFile(idFile).getLines()) {
      // 1st column: PMC number id; 3rd column: title
      val bits = l.split("\\s+")
      h += bits(2) -> bits(0)
    }
    h.toMap
  }

  def loadBigMechIds(bmFile:String): Set[String] = {
    val s = new mutable.HashSet[String]()
    for(l <- io.Source.fromFile(bmFile).getLines()) {
      if(l.startsWith("PMC")) s += l.trim
    }
    s.toSet
  }

  def findFiles(dir:String, ext:String):List[File] = {
    val papers = new ListBuffer[File]

    // find all files ending with ext in this directory
    val fileNameFilter = new FilenameFilter {
      override def accept(file: File, name: String): Boolean = {
        name.toLowerCase.endsWith("." + ext)
      }
    }
    papers ++= new File(dir).listFiles(fileNameFilter).toList

    // recursive call
    val dirNameFilter = new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = {
        val file = new File(dir.getAbsolutePath + File.separator + name)
        file.isDirectory
      }
    }
    val subdirs = new File(dir).listFiles(dirNameFilter)
    for(subdir <- subdirs) {
      papers ++= findFiles(subdir.getAbsolutePath, ext)
    }

    papers.toList
  }

  /**
   * Extracts the prefix of a file name, where file name is prefix.nxml
   */
  def extractPrefix(paper:File, ext:String):String = {
    val fn = paper.getName
    assert(fn.endsWith("." + ext))
    fn.substring(0, fn.length - 5)
  }

  def main(args:Array[String]) {
    val props = argsToProperties(args)

    val pubmedDir = props.getProperty("pubmed.dir")
    val fileIdsFile = props.getProperty("id.file")
    val bigMechIdsFile = props.getProperty("bigmech.file")

    val fileIds = loadMappings(fileIdsFile)
    println(s"Loaded a total of ${fileIds.size} PMC numbers.")
    val bigMechIds = loadBigMechIds(bigMechIdsFile)
    println(s"The BigMech corpus contains ${bigMechIds.size} documents.")
    val files = findFiles(pubmedDir, "nxml")
    println(s"Read ${files.size} files from the pubmed directory.")

    val os = new PrintStream(new FileOutputStream("bigmech.txt"))
    var count = 0
    for(f <- files) {
      val prefix = extractPrefix(f, "nxml")
      val id = fileIds.get(prefix)
      if(id.isDefined) {
        if(bigMechIds.contains(id.get)) {
          os.println(f)
          count += 1
        }
      }
    }
    os.close()
    println(s"Found ${count} files in the BigMech corpus. They were all saved in bigmech.txt.")
  }

}
