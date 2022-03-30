package ru.dokwork.spellchecklsp

import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.Position
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TextSpec extends AnyFreeSpec with Matchers:

  val EOL = System.lineSeparator

  "Reading" - {
    "text with empty string should be empty" in {
      "".toText.isEmpty shouldBe true
    }

    "empty text should not contain any line" in {
      val text = "".toText
      text.linesCount shouldBe 0
      text.lines shouldBe empty
      text.line(0) shouldBe empty
    }

    "Text with EOL in the end should be terminated" in {
      "".toText.isTerminated shouldBe false
      EOL.toText.isTerminated shouldBe true
    }

    "EOL should terminate lines" in {
      // see https://en.wikipedia.org/wiki/Newline#Interpretation
      EOL.toText.linesCount shouldBe 1
    }
  }

  "Getting line" - {
    "line can be gote by its zero-based number" in {
      // given:
      val text = """This is a text
                  |with two lines""".stripMargin.toText

      // then:
      text.line(0) shouldBe Some("This is a text" + EOL)
      text.line(1) shouldBe Some("with two lines")
    }

    "None should be returned when line is absent" in {
      "Single line text".toText.line(1) shouldBe None
    }
  }

  "Substring" - {
    "should return substring from the specified range (exclusive the last symbol)" in {
      // given:
      val text  = s"""This is a text
                  |with two lines""".stripMargin
      val range = Range(Position(0, text.indexOf("text")), Position(1, 4))

      // when:
      val result = text.toText.substring(range)

      // then:
      result shouldBe """text
                        |with""".stripMargin
    }

    "should return empty string for a range where the start is out of text" in {
      // given:
      val text   = "test"
      val range1 = Range(Position(0, text.length), Position(0, text.length))
      val range2 = Range(Position(1, 0), Position(1, 0))
      // then:
      text.toText.substring(range1) shouldBe empty
      text.toText.substring(range2) shouldBe empty
    }

    "should return substring from the start postion to the end of text when end of range is out of text" in {
      // given:
      val text  = "test"
      val range = Range(Position(0, 3), Position(0, text.length + 1))
      // then:
      text.toText.substring(range) shouldBe "t"
    }
  }

  "Apply change" - {
    "should append the symbol to the end" in {
      // given:
      val text                    = "test"
      val range                   = Range(Position(0, 4), Position(0, 4))
      // when:
      val (result, affectedLines) = text.toText.change(range, "!")
      // then:
      result.plainText shouldBe "test!"
      affectedLines shouldBe Updated(0 to 0)
    }

    "should replace the symbol" in {
      // given:
      val text                    = "test"
      val range                   = Range(Position(0, 2), Position(0, 3))
      // when:
      val (result, affectedLines) = text.toText.change(range, "X")
      // then:
      result.plainText shouldBe "teXt"
      affectedLines shouldBe Updated(0 to 0)
    }

    "should delete the symbol" in {
      // given:
      val text                    = "test"
      val range                   = Range(Position(0, 2), Position(0, 3))
      // when:
      val (result, affectedLines) = text.toText.change(range, "")
      // then:
      result.plainText shouldBe "tet"
      affectedLines shouldBe Updated(0 to 0)
    }

    "should append the new line" in {
      // given:
      val text                    = "test"
      val newLine                 = "new line"
      val range                   = Range(Position(1, 0), Position(1, 0))
      // when:
      val (result, affectedLines) = text.toText.change(range, newLine + EOL)
      // then:
      result.plainText shouldBe text + EOL + newLine + EOL
      affectedLines shouldBe Inserted(1 to 1)
    }

    "should prepend the new line" in {
      // given:
      val text                    = "test"
      val newLine                 = "new line"
      val range                   = Range(Position(0, 0), Position(0, 0))
      // when:
      val (result, affectedLines) = text.toText.change(range, newLine + EOL)
      // then:
      result.plainText shouldBe newLine + EOL + text
      affectedLines shouldBe Inserted(0 to 0)
    }

    "should delete the line" in {
      // given:
      val text                    = "multi line" + EOL + "test"
      val range                   = Range(Position(0, 0), Position(1, 0))
      // when:
      val (result, affectedLines) = text.toText.change(range, "")
      // then:
      result.plainText shouldBe "test"
      affectedLines shouldBe Removed(0 to 0)
    }
  }
