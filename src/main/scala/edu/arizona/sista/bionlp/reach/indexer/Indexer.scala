package edu.arizona.sista.bionlp.reach.indexer

import java.io.{FileReader, BufferedReader, FilenameFilter, File}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Trait for all pubmed indexers
 * User: mihais
 * Date: 11/5/14
 */
trait Indexer {
	/**
	 * Indexes all .nxml files in paperDirectory to a Lucene index stored in indexDirectory; File names are mapped to ids using idFile.
	 */
	def index(paperDirectory:String,
						indexDirectory:String,
						idFile:String)

	def loadMappings(idFile:String):Map[String, String] = {
		val h = new mutable.HashMap[String, String]
		for(l <- io.Source.fromFile(idFile).getLines()) {
			// 2nd column: pubmed id; 3rd column: title
			val bits = l.split("\\s+")
			h += bits(2) -> bits(1)
		}
		h.toMap
	}

	def fetchPaperId(name:String, nameToId:Map[String, String]):String = {
		// if it exists in the mappings file, great
		val rn = mkReadable(name)
		val id = nameToId.get(rn)
		if(id.isDefined) {
			id.get
		} else {
			val aid = mkIdFromName(name)
			aid
		}
	}

	def mkReadable(name:String):String = {
		// replaces %28 with (
		// replaces $29 with )
		var readable = name.replaceAll("\\%28", "(")
		readable = readable.replaceAll("\\%29", ")")
		readable
	}

	/** Makes an artificial id from a paper name by keeping only alphanum characters */
	def mkIdFromName(name:String):String = {
		val b = new mutable.StringBuilder()
		for(i <- 0 until name.length) {
			if(Character.isLetterOrDigit(name.charAt(i)))
				b += name.charAt(i)
		}
		b.toString()
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

	def mkFileName(dir:String, prefix:String, section:String, ext:String):String = {
		dir + File.separator + prefix + "." + section + "." + ext
	}

	/** Reads a text file into a string */
	def textFileToString(fn:String):String = {
		val b = new BufferedReader(new FileReader(fn))
		val out = new StringBuilder
		var done = false
		while(! done) {
			val l = b.readLine()
			if(l == null) {
				done = true
			} else {
				out.append(l)
				out.append("\n")
			}
		}
		b.close()
		out.toString()
	}
}
