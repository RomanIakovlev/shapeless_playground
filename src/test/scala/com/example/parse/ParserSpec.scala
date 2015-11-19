package com.example.parse

import com.example.parse.model.{BookBook, Person}
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  "simple case class" should "be correctly parsed" in {
    val parseResult = Parser[Person]("Amy,54.2")
    parseResult should not be empty
    parseResult.get should equal (Person("Amy", 54.2))
  }

  "nested case classes" should "compile" in {
    Parser[BookBook]("Hello,World")
  }
}
