package edu.arizona.sista.bionlp

import java.io.File
import edu.arizona.sista.bionlp.mentions.Display
import jline.console.ConsoleReader
import jline.console.history.FileHistory
import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document

object ReachShell extends App {

  println("Loading ReachSystem ...")
  val reach = new ReachSystem

  val history = new FileHistory(new File(System.getProperty("user.home"), ".reachshellhistory"))
  sys addShutdownHook {
    history.flush() // we must flush the file before exiting
  }

  val reader = new ConsoleReader
  reader.setPrompt(">>> ")
  reader.setHistory(history)

  val commands = Map(
    "%help" -> "show commands",
    "%exit" -> "exit system"
  )

  println(s"\nWelcome to ReachShell!\n")
  printCommands()

  var running = true

  while (running) {
    reader.readLine match {
      case "%help" =>
        printCommands()

      case "%exit" | null =>
        running = false

      case text =>
        val doc = reach.mkDoc(text, "rulershell")
        val mentions = reach.extractFrom(doc)
        displayMentions(mentions, doc)
    }
  }

  // manual terminal cleanup
  reader.getTerminal().restore()
  reader.shutdown()


  // functions

  def printCommands(): Unit = {
    println("COMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println
  }

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText())
      println
      mentionsBySentence(i).sortBy(_.label) foreach displayMention
      println("=" * 50)
    }
  }

  def displayMention(mention: Mention) {
    val boundary = s"\t${"-" * 30}"
    println(mention.labels)
    println(boundary)
    println(s"\tRule => ${mention.foundBy}")
    val mentionType = mention.getClass.toString.split("""\.""").last
    println(s"\tType => $mentionType")
    println(boundary)
    mention match {
      case m: TextBoundMention =>
        println(s"\t${m.asInstanceOf[Display].displayLabel}|${m.labels} => ${m.text}")
      case m: EventMention =>
        println(s"\ttrigger => ${m.trigger.text}")
        m.arguments foreach {
          case (k, vs) => for (v <- vs) println(s"\t$k (${v.labels}) => ${v.text}")
        }
      case m: RelationMention =>
        m.arguments foreach {
          case (k, vs) => for (v <- vs) println(s"\t$k (${v.labels}) => ${v.text}")
        }
      case _ => ()
    }
    println(s"$boundary\n")
  }
}
