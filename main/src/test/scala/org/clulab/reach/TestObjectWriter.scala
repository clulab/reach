package org.clulab.reach

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sink
import org.json4s.{JArray, JObject, JValue}
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods
import org.scalatest.{FlatSpec, Matchers}

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets

class TestObjectWriter extends FlatSpec with Matchers {

  behavior of "ObjectWriter"

  it should "produce a small file" in {
    val json = """{ "boolean": true, "integer": 42, "float": 3.14, "string": "Hello, world!" }"""
    val jValue = JsonMethods.parse(json)
    val renderedJValue = JsonMethods.render(jValue)
    val objectWriter = JsonMethods.mapper.writerWithDefaultPrettyPrinter()
    val file = new File("SmallTestObjectWriter.json")
    val printWriter = new PrintWriter(new Sink(file, StandardCharsets.UTF_8.name, append = false))

    printWriter.autoClose { printWriter =>
      objectWriter.writeValue(printWriter, renderedJValue)
    }
    file.exists should be (true)
    file.delete()
  }

  // This will produce a file of about 8GB!
  ignore should "produce a very large file" in {
    val jObject =
        ("string" -> "The quick brown fox jumped over the lazy dog.") ~
        ("number" -> 42)
    val list = 1.to(100000000).map(_ => jObject).toList
    val jArray = JArray(list)
    val jValue = jArray
    val renderedJValue = JsonMethods.render(jValue)
    val objectWriter = JsonMethods.mapper.writerWithDefaultPrettyPrinter()
    val file = new File("LargeTestObjectWriter.json")
    val printWriter = new PrintWriter(new Sink(file, StandardCharsets.UTF_8.name, append = false))

    printWriter.autoClose { printWriter =>
      objectWriter.writeValue(printWriter, renderedJValue)
    }
    file.exists should be(true)
    file.delete()
  }
}
