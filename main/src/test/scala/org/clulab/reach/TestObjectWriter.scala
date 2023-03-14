package org.clulab.reach

import com.fasterxml.jackson.databind.ObjectMapper
import org.clulab.utils.Sink
import org.json4s.jackson.JsonMethods
import org.scalatest.{FlatSpec, Matchers}

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets

class TestObjectWriter extends FlatSpec with Matchers {

  behavior of "ObjectWriter"

  it should "produce a file" in {
    val json = """{ "boolean": true, "integer": 42, "float": 3.14, "string": "Hello, world!" }"""
    val jValue = JsonMethods.parse(json)
    println(jValue)
    val prettyJson = JsonMethods.pretty(JsonMethods.render(jValue))
    println(prettyJson)

    val file = new File("TestObjectWriter.json")
    val printWriter = new PrintWriter(new Sink(file, StandardCharsets.UTF_8.name, append = false))
    val objectMapper = new ObjectMapper()
    val objectWriter = objectMapper.writerWithDefaultPrettyPrinter()

    objectWriter.writeValue(printWriter, jValue)
  }
}
