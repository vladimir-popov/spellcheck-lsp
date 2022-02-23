package ru.dokwork.spellchecklsp

import java.util.{ List => JList, Map => JMap }
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.*

import org.eclipse.lsp4j.{ CodeAction, Position, Range, TextDocumentContentChangeEvent, TextEdit }

import org.languagetool.language.AmericanEnglish
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inspectors

class TxtDocumentSpec extends AnyFreeSpec with Matchers with Inspectors with TestCases:

  "Splitting lines" - {
    "should return empty seq" in {
      TxtDocument.splitLines("") shouldBe empty
    }
  }

  "Applying changes" - {

    "one line change" in {
      // when:
      val result = Text.doc.applyChange(Text.Changes.fixMistake1)
      // then:
      result.text shouldBe
        s"""This is a text
          |with two ${Text.Mistakes.mmistakes}""".stripMargin
    }

    "append text" in {
      // when:
      val result = Text.doc.applyChange(Text.Changes.appendDotToEnd)
      // then:
      result.text shouldBe
        s"""This is a text
           |with wto mmistakes.""".stripMargin
    }

    "add new line" in {
      // when:
      val result = Text.doc.applyChange(Text.Changes.addNewLine)
      // then:
      result.text shouldBe
        """This is a text
          |with wto mmistakes
          |for tests""".stripMargin
    }

    "delete line" in {
      // when:
      val result = Text.doc.applyChange(Text.Changes.deleteLastLine)
      // then:
      result.text shouldBe Text.line0
    }

    "merge lines" in {
      // when:
      val result = Text.doc.applyChange(Text.Changes.mergeLines)
      // then:
      result.text shouldBe
        """This is a text
          |without mistakes
          |for tests""".stripMargin
    }
  }

  "Check spelling" - {
    "should find two mistakes" in {
      // given:
      val textWith2Mistakes = Text.doc.text
      val p1                = textWith2Mistakes.indexOf(Text.Mistakes.wto)
      val p2                = textWith2Mistakes.indexOf(Text.Mistakes.mmistakes)

      // when:
      val suggestions = TxtDocument.check(ArraySeq(textWith2Mistakes), AmericanEnglish())

      // then:
      suggestions(0).map(_.range) should contain allOf (
        Range(Position(0, p1), Position(0, p1 + Text.Mistakes.wto.length)),
        Range(Position(0, p2), Position(0, p2 + Text.Mistakes.mmistakes.length))
      )
    }
  }

  "Diagnostics" - {

    "empty diagnostic for a correct text" in {
      val txt = TxtDocument.create("Hello world")
      txt.diagnostics.toIndexedSeq shouldBe empty
    }

    "should be created diagnostic for both mistakes" in {
      // when:
      val diagnostics = Text.doc.diagnostics.toIndexedSeq
      // then:
      diagnostics should have size 2
    }

    "diagnostics should have correct line number" in {
      // when:
      val diagnostics = Text.doc.diagnostics.toIndexedSeq

      // then:
      diagnostics.map(_.getRange.getStart.getLine) should contain only (1)
      diagnostics.map(_.getRange.getEnd.getLine) should contain only (1)
    }

    "diagnostic should have right range" in {
      // when:
      val diagnostic = Text.doc.diagnostics.toIndexedSeq.head
      // then:
      diagnostic.getRange shouldBe Range(
        Position(1, Text.line1.indexOf(Text.Mistakes.wto)),
        Position(1, Text.line1.indexOf(Text.Mistakes.wto) + 3)
      )
    }

    "diagnostics should be removed after applying cnahges" in {
      // when:
      val diagnostics = Text.doc
        .applyChange(Text.Changes.fixMistake1)
        .applyChange(Text.Changes.fixMistake2)
        .diagnostics
        .toIndexedSeq

      // then:
      diagnostics shouldBe empty
    }

    "should be added one more diagnostic for the new added line" in {
      // when:
      val diagnostics =
        Text.doc.applyChange(Text.Changes.addNewLineWithMistake).diagnostics.toIndexedSeq
      // then:
      diagnostics.filter(_.getRange.getStart.getLine == 0) should have size 0
      diagnostics.filter(_.getRange.getStart.getLine == 1) should have size 2
      diagnostics.filter(_.getRange.getStart.getLine == 2) should have size 1
    }
  }

  "Text edits" - {
    // given:
    val char = Text.line1.indexOf(Text.Mistakes.wto)
    val p1   = Position(1, char)
    val p2   = Position(1, char + Text.Mistakes.wto.length)

    "should return no more than 10 edits" in {
      // when:
      val edits = Text.doc.getTextEdits(p1)
      // then:
      edits.toJList.size should be <= 10
    }

    "should return edits to fix only first mistake" in {
      // when:
      val edits = Text.doc.getTextEdits(p1).toIndexedSeq
      // then:
      forAll(edits)(_.getRange shouldBe Range(p1, p2))
    }
  }

trait TestCases:

  object Text:
    object Mistakes:
      val wto       = "wto"
      val mmistakes = "mmistakes"
      val tsts      = "tsts"

    val line0              = "This is a text"
    val line1              = s"with ${Mistakes.wto} ${Mistakes.mmistakes}"
    val newLine            = "\nfor tests"
    val newLineWithMistake = s"\nfor ${Mistakes.tsts}"

    /** {{{
      *   This is a text
      *   with wto mmistakes
      * }}}
      */
    val doc: TxtDocument = TxtDocument.create(line0 + "\n" + line1)

    object Changes:
      val fixMistake1           = TextDocumentContentChangeEvent(
        Range(
          Position(1, line1.indexOf(Mistakes.wto)),
          Position(1, line1.indexOf(Mistakes.wto) + Mistakes.wto.length)
        ),
        "two".length,
        "two"
      )
      val fixMistake2           = TextDocumentContentChangeEvent(
        Range(
          Position(1, line1.indexOf(Mistakes.mmistakes)),
          Position(1, line1.indexOf(Mistakes.mmistakes) + Mistakes.mmistakes.length)
        ),
        "mistakes".length,
        "mistakes"
      )
      val appendDotToEnd        = TextDocumentContentChangeEvent(
        Range(Position(1, line1.length), Position(1, line1.length)),
        0,
        "."
      )
      val addNewLine            = TextDocumentContentChangeEvent(
        Range(Position(1, line1.length), Position(2, newLine.length - 1)),
        newLine.length,
        newLine
      )
      val addNewLineWithMistake = TextDocumentContentChangeEvent(
        Range(Position(1, line1.length), Position(2, newLineWithMistake.length - 1)),
        newLineWithMistake.length,
        newLineWithMistake
      )
      val deleteLastLine        = TextDocumentContentChangeEvent(
        Range(Position(1, 0), Position(2, 0)),
        line1.length,
        ""
      )
      val mergeLines            = TextDocumentContentChangeEvent(
        Range(Position(1, "with".length), Position(2, newLine.length)),
        ("out mistakes" + newLine).length,
        "out mistakes" + newLine
      )
