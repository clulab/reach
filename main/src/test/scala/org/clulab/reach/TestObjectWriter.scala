package org.clulab.reach

import com.fasterxml.jackson.databind.ObjectMapper
import org.clulab.utils.Sink
import org.json4s.jackson.{JsonMethods, renderJValue}
import org.scalatest.{FlatSpec, Matchers}

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets

class TestObjectWriter extends FlatSpec with Matchers {

  behavior of "ObjectWriter"

  it should "produce a file" in {
    val json = """{ "boolean": true, "integer": 42, "float": 3.14, "string": "Hello, world!" }"""
    val jValue = JsonMethods.parse(json)
    val renderedJValue = JsonMethods.render(jValue)
    println(jValue)
    println(renderedJValue)

    val prettyJson = JsonMethods.pretty(renderedJValue)
    println(prettyJson)

    val file = new File("TestObjectWriter.json")
    val printWriter = new PrintWriter(new Sink(file, StandardCharsets.UTF_8.name, append = false))
//    val objectMapper = new ObjectMapper()
//    val objectWriter = objectMapper.writerWithDefaultPrettyPrinter()
    // This actually writes to the file, but not pretty.
//    val objectWriter = JsonMethods.mapper.writer()
    val objectWriter = JsonMethods.mapper.writerWithDefaultPrettyPrinter()

    objectWriter.writeValue(printWriter, renderedJValue)

    val string = JsonMethods.pretty(renderedJValue)
    println(string)
  }
}
