package ru.dokwork.spellchecklsp

import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.Position
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TextSpec extends AnyFreeSpec with Matchers:

  "Read" - {
    "should return line by its zero-based number" in {
      // given:
      val text = """This is a text
                  |with two lines""".stripMargin.asText

      // then:
      text.line(0) shouldBe "This is a text\n"
      text.line(1) shouldBe "with two lines"
    }

    "should return empty line if line is absent" in {
      "Single line text".asText.line(1) shouldBe empty
    }

    "should return substring from the specified range" in {
      // given:
      val text  = s"""This is a text
                  |with two lines""".stripMargin
      val range = Range(Position(0, text.indexOf("text")), Position(1, 4))

      // when:
      val result = text.asText.substring(range)

      // then:
      result shouldBe """text
                        |with""".stripMargin
    }

    "should return empty if range begins out of text" in {
      // given:
      val text  = "test"
      val range1 = Range(Position(0, text.length), Position(0, text.length))
      val range2 = Range(Position(1, 0), Position(1, 0))
      // then:
      text.asText.substring(range1) shouldBe empty
      text.asText.substring(range2) shouldBe empty
    }
  }

  "Apply change" - {
    "should append symbol" in {
      // given:
      val range                  = Range(Position(0, 4), Position(0, 4))
      // when:
      val (result, changedLines) = "text".asText.change(range, "!")
      // then:
      result.plainText shouldBe "text!"
      changedLines shouldBe (0 to 0)
    }

    "should replace symbol" in {
      // given:
      val text = "test"
      val range = Range(Position(0, 2), Position(0, 3))
      // when:
      val (result, _) = text.asText.change(range, "X")
      // then:
      result.plainText shouldBe "teXt"
    }

    "should append new line" in {
      // given:
      val text = "test"
      val newLine = "\nnew line"
      val range                  = Range(Position(0, text.length), Position(0, text.length))
      // when:
      val (result, changedLines) = text.asText.change(range, newLine)
      // then:
      result.plainText shouldBe text + newLine
      changedLines shouldBe (1 to 1)
    }

    "should prepend new line" in {
      // given:
      val text = "test"
      val newLine = "new line\n"
      val range                  = Range(Position(0, 0), Position(0, 0))
      // when:
      val (result, changedLines) = text.asText.change(range, newLine)
      // then:
      result.plainText shouldBe newLine + text
      changedLines shouldBe (0 to 0)
    }

    "should delete line" in {
      // given:
      val text = "multi line\ntest"
      val range                  = Range(Position(0, 0), Position(1, 0))
      // when:
      val (result, changedLines) = text.asText.change(range, "")
      // then:
      result.plainText shouldBe "test"
      changedLines shouldBe (0 to 0)
    }
  }
