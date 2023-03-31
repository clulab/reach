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

    if (!result)
      println("Processors mentions are not equal!  Do something about it!")
    result
  }

  def testReachSerialization(mentions: Seq[Mention]): Boolean = {
    import org.clulab.reach.mentions.serialization.json.JSONSerializer

    val json = MentionsOps(mentions).json(pretty = true)

    val mentions2 = JSONSerializer.toCorefMentions2(parse(json))
    val json2 = MentionsOps(mentions2).json(pretty = true)
    val result = json == json2

    if (!result)
      println("Reach mentions are not equal!  Do something about it!")
    result
  }

  var failCount = 0

  files.par.foreach { file =>
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)

    val result1 = testProcessorsSerialization(mentions)
    val result2 = testReachSerialization(mentions)

    if (!(result1 && result2)) {
      println(s"File ${file.getName} failed.")
      failCount += 1
    }
  }
  if (failCount != 0)
    println(s"There were $failCount failures!")
}
