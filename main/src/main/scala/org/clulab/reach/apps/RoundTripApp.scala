package org.clulab.reach.apps

import org.clulab.odin.Mention
import org.clulab.reach.PaperReader
import org.clulab.reach.mentions.serialization.json.MentionsOps
import org.clulab.serialization.json.stringify
import org.clulab.utils.FileUtils
import org.json4s.jackson.JsonMethods.parse

object RoundTripApp extends App {
  val directoryName = args.headOption.getOrElse("../corpora/nxml")
  val files = FileUtils.findFiles(directoryName, ".nxml")

  def testProcessorsSerialization(mentions: Seq[Mention]): Boolean = {
    import org.clulab.odin.serialization.json.JSONSerializer

    val jValue = JSONSerializer.jsonAST(mentions)
    val json = stringify(jValue, pretty = true)

    val mentions2 = JSONSerializer.toMentions(parse(json))
    val jValue2 = JSONSerializer.jsonAST(mentions2)
    val json2 = stringify(jValue2, pretty = true)
    val result = json == json2

    if (json != json2) {
      println("Processors mentions are not equal!  Do something about it!")
      println(json)
      println(json2)
    }
    result
  }

  def testReachSerialization(mentions: Seq[Mention]): Boolean = {
    import org.clulab.reach.mentions.serialization.json.JSONSerializer

    val json = MentionsOps(mentions).json(pretty = true)

    val mentions2 = JSONSerializer.toCorefMentions(parse(json))
    val json2 = MentionsOps(mentions2).json(pretty = true)
    val result = json == json2

    if (json != json2) {
      println("Reach mentions are not equal!  Do something about it!")
      println(json)
      println(json2)
    }
    result
  }

  files.foreach { file =>
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)

    testProcessorsSerialization(mentions)
    testReachSerialization(mentions)
  }
}
