package edu.arizona.sista.reach

import java.io.{File, FileWriter}
import scala.language.reflectiveCalls

package object brat {
  type Closeable = { def close(): Unit }

  def using[A <: Closeable, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  def readFile(filename: String): String =
    using (io.Source.fromFile(filename)) {
      source => source.mkString
    }

  def readFile(file: File): String =
    using (io.Source.fromFile(file)) {
      source => source.mkString
    }

  def writeFile(file: File, text: String) {
    using (new FileWriter(file)) {
      writer => writer.write(text)
    }
  }

  def writeFile(filename: String, text: String) {
    writeFile(new File(filename), text)
  }
}
