package ru.dokwork.spellchecklsp

import java.util.{ List => JList, Map => JMap }
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.*

import org.eclipse.lsp4j.{ CodeAction, Position, Range, TextDocumentContentChangeEvent, TextEdit }

import org.languagetool.language.AmericanEnglish
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inspectors

class TxtDocumentSpec extends AnyFreeSpec with Matchers with Inspectors:

  "Check spelling" - {
    "should find two mistakes" in {
      // given:
      val m1                = "mistke"
      val m2                = "mstake"
      val textWith2Mistakes = s"Text with two mistakes: $m1, $m2".toText
      val p1                = textWith2Mistakes.toString.indexOf(m1)
      val p2                = textWith2Mistakes.toString.indexOf(m2)

      // when:
      val suggestions = TxtDocument.check(textWith2Mistakes, AmericanEnglish())

      // then:
      suggestions(0).map(_.range) should contain allOf (
        Range(Position(0, p1), Position(0, p1 + m1.length)),
        Range(Position(0, p2), Position(0, p2 + m2.length))
      )
    }
  }

  "Diagnostics" - {

    "diagnostics should be empty for a correct text" in {
      val text = TxtDocument("Hello world")
      text.diagnostics.toIndexedSeq shouldBe empty
    }

    "should be created diagnostic for both mistakes" in {
      // given:
      val m1                = "mistke"
      val m2                = "mstake"
      val textWith2Mistakes = s"Text with two mistakes: $m1, $m2"
      // when:
      val diagnostics       = TxtDocument(textWith2Mistakes).diagnostics.toIndexedSeq
      // then:
      diagnostics should have size 2
    }

    "diagnostics should have correct line number" in {
      // given:
      val m    = "mistke"
      val text = s"Text with mistake:\n$m"

      // when:
      val diagnostics = TxtDocument(text).diagnostics.headOption

      // then:
      // only last line has mistakes
      diagnostics.map(_.getRange.getStart.getLine) should contain(1)
      diagnostics.map(_.getRange.getEnd.getLine) should contain(1)
    }

    "diagnostic should have right range" in {
      // given:
      val m          = "mistke"
      val text       = s"Text with mistake: $m"
      // when:
      val diagnostic = TxtDocument(text).diagnostics.headOption
      // then:
      val character  = text.indexOf(m)
      diagnostic.map(_.getRange) should contain(Range(
        Position(0, character),
        Position(0, character + m.length)
      ))
    }

    "diagnostics should be removed after applying cnahge" in {
      // given:
      val m      = "mistke"
      val text   = s"Text with mistake: $m"
      val change = changeEvent(
        Range(
          Position(0, text.indexOf(m)),
          Position(0, text.indexOf(m) + m.length)
        ),
        "mistake"
      )
      // when:
      val diagnostics = TxtDocument(text)
        .applyChange(change)
        .diagnostics
        .toIndexedSeq
      // then:
      diagnostics shouldBe empty
    }

    "should be added a diagnostic for the new added line" in {
      // give:
      val text                     = "Text without mistakes."
      val appendNewLineWithMistake = changeEvent(
        Range(Position(1, 0), Position(1, 0)),
        "New line with mstk." + System.lineSeparator
      )
      // when:
      val diagnostics              =
        TxtDocument(text).applyChange(appendNewLineWithMistake).diagnostics.toIndexedSeq
      // then:
      diagnostics.filter(_.getRange.getStart.getLine == 0) should have size 0
      diagnostics.filter(_.getRange.getStart.getLine == 1) should have size 1
    }
  }

  "Text edits" - {

    "should return limited count of `TextEdit`s" in {
      // given:
      val N     = 5
      val m = "mstk"
      val text = s"Text with $m."
      val p = Position(0, text.indexOf(m))
      // when:
      val edits = TxtDocument(text).getTextEdits(p, N)
      // then:
      edits.toJList.size should be <= N
    }

    "should return edits to fix only the first mistake" in {
      // given:
      val m1                = "mistke"
      val m2                = "mstake"
      val text = s"Text with two mistakes: $m1, $m2"
      val p1 = Position(0, text.indexOf(m1))
      val p2 = Position(0, text.indexOf(m1) + m1.length)
      // when:
      val edits = TxtDocument(text).getTextEdits(p1).toIndexedSeq
      // then:
      forAll(edits)(_.getRange shouldBe Range(p1, p2))
    }
  }


  private def changeEvent(range: Range, text: String) =
    TextDocumentContentChangeEvent(range, 0, text)
