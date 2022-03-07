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

  "Check spelling" - {
    "should find two mistakes" in {
      // given:
      val textWith2Mistakes = Text.doc.text
      val p1                = textWith2Mistakes.indexOf(Text.Mistakes(0))
      val p2                = textWith2Mistakes.indexOf(Text.Mistakes(1))

      // when:
      val suggestions = TxtDocument.check(ArraySeq(textWith2Mistakes), AmericanEnglish())

      // then:
      suggestions(0).map(_.range) should contain allOf (
        Range(Position(0, p1), Position(0, p1 + Text.Mistakes(0).length)),
        Range(Position(0, p2), Position(0, p2 + Text.Mistakes(1).length))
      )
    }
  }

  "Diagnostics" - {

    "diagnostics should be empty for a correct text" in {
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
      // only last line has mistakes
      diagnostics.map(_.getRange.getStart.getLine) should contain only (1)
      diagnostics.map(_.getRange.getEnd.getLine) should contain only (1)
    }

    "diagnostic should have right range" in {
      // when:
      val diagnostic = Text.doc.diagnostics.toIndexedSeq.head
      // then:
      val character = Text.line1.indexOf(Text.Mistakes(0))
      diagnostic.getRange shouldBe Range(
        Position(1, character),
        Position(1, character + Text.Mistakes(0).length)
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
      // give:
      val appendNewLineWithMistake = changeEvent(
        Range(Position(2, 0), Position(2, 0)),
        "new line with mstk" + System.lineSeparator
      )
      // when:
      val diagnostics =
        Text.doc.applyChange(appendNewLineWithMistake).diagnostics.toIndexedSeq
      // then:
      diagnostics.filter(_.getRange.getStart.getLine == 0) should have size 0
      diagnostics.filter(_.getRange.getStart.getLine == 1) should have size 2
      diagnostics.filter(_.getRange.getStart.getLine == 2) should have size 1
    }
  }

  "Text edits" - {
    // given:
    val char = Text.line1.indexOf(Text.Mistakes(0))
    val p1   = Position(1, char)
    val p2   = Position(1, char + Text.Mistakes(0).length)

    "should return limited count of `TextEdit`s" in {
      // given:
      val N = 5
      // when:
      val edits = Text.doc.getTextEdits(p1, N)
      // then:
      edits.toJList.size should be <= N
    }

    "should return edits to fix only the first mistake" in {
      // when:
      val edits = Text.doc.getTextEdits(p1).toIndexedSeq
      // then:
      forAll(edits)(_.getRange shouldBe Range(p1, p2))
    }
  }

trait TestCases:

  def changeEvent(range: Range, text: String) =
    TextDocumentContentChangeEvent(range, 0, text)

  object Text:
    object Mistakes:
      val wto       = "wto"
      val mmistakes = "mmistakes"
      def apply(i: Int) = if i == 0 then wto else mmistakes

    val line0              = "This is a text"
    val line1              = s"with ${Mistakes.wto} ${Mistakes.mmistakes}"

    /** {{{
      *   This is a text
      *   with wto mmistakes
      * }}}
      */
    val doc: TxtDocument = TxtDocument.create(line0 + "\n" + line1)

    object Changes:
      val fixMistake1           = changeEvent(
        Range(
          Position(1, line1.indexOf(Mistakes.wto)),
          Position(1, line1.indexOf(Mistakes.wto) + Mistakes.wto.length)
        ),
        "two"
      )
      val fixMistake2           = changeEvent(
        Range(
          Position(1, line1.indexOf(Mistakes.mmistakes)),
          Position(1, line1.indexOf(Mistakes.mmistakes) + Mistakes.mmistakes.length)
        ),
        "mistakes"
      )
      val appendDotToEnd        = changeEvent(
        Range(Position(1, line1.length), Position(1, line1.length)),
        "."
      )
      val deleteLastLine        = changeEvent(
        Range(Position(1, 0), Position(2, 0)),
        ""
      )
