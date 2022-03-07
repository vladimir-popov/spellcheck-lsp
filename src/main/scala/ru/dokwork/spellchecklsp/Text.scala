package ru.dokwork.spellchecklsp

import scala.annotation.tailrec
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.Position
import scala.Range.{ Exclusive => ExclusiveRange, Inclusive => InclusiveRange }
import Change.*
import com.typesafe.scalalogging.StrictLogging

case class Text(plainText: String)(lines: Map[Int, Int]) extends StrictLogging:

  @inline
  private def EOL = System.lineSeparator

  def isEmpty = plainText.isEmpty

  def nonEmpty = !isEmpty

  /** Returns true if the last symbol in the text is EOL. */
  def isTerminated: Boolean = plainText.endsWith(EOL)

  /** Returns line with number `i` if it exists, or None.
    * @param i
    *   zero-based number of line.
    */
  def line(i: Int): Option[String] =
    val maybeFrom = lines.get(i)
    val to        = lines.getOrElse(i + 1, plainText.length)
    maybeFrom.map(from => plainText.slice(from, to))

  /** Returns count of lines in this text. Empty text has one line. */
  def linesCount: Int = lines.size max 1

  def substring(range: Range): String =
    val from = indexOf(range.getStart)
    val to   = indexOf(range.getEnd)
    plainText.slice(from, to)

  def change(range: Range, newText: String): (Text, AffectedLines) =
    val ch = Change(range, newText.toText)
    logger.trace(s"Applying $ch")
    apply(ch)

  private def apply: Change => (Text, AffectedLines) =
    case Insert(line, text)              =>
      insert(line, text) -> Inserted(line to (line + text.linesCount - 1))
    case Remove(from, until)             =>
      remove(from, until) -> Removed(from to (until - 1))
    case Update(line, from, until, text) =>
      update(line, from, until, text) -> Updated(line to (line + text.linesCount - 1))
    case unsupported: UnsupportedChange  =>
      throw unsupported

  private def insert(line: Int, newText: Text): Text =
    val str =
      lines.get(line) match
        case Some(index) =>
          plainText.slice(0, index) + newText.plainText + plainText.slice(index, plainText.length)
        case None        =>
          plainText + EOL + newText.plainText
    str.toText

  private def update(line: Int, fromChar: Int, untilChar: Int, text: Text): Text =
    val from  = lines(line) + fromChar
    val until = lines(line) + untilChar
    (plainText.slice(0, from) + text.plainText + plainText.slice(until, plainText.length)).toText

  private def remove(fromLine: Int, toLine: Int): Text =
    val from = lines(fromLine)
    val to   = lines(toLine)
    (plainText.slice(0, from) + plainText.slice(to, plainText.length)).toText

  private def indexOf(pos: Position): Int = lines.get(pos.getLine).map(
    _ + pos.getCharacter
  ).getOrElse(plainText.length)

extension (str: String)
  def toText: Text =
    val EOL                                                                  = System.lineSeparator
    @tailrec
    def loop(line: Int, character: Int, lines: Map[Int, Int]): Map[Int, Int] =
      if character >= 0 && character < str.length && !lines.contains(character) then
        loop(line + 1, str.indexOf(EOL, character) + 1, lines + (line -> character))
      else
        lines

    Text(str)(loop(0, 0, Map.empty))

sealed trait Change

object Change:
  case class Insert(line: Int, newText: Text)                             extends Change
  case class Remove(fromLine: Int, untilLine: Int)                        extends Change
  case class Update(line: Int, fromChar: Int, untilChar: Int, text: Text) extends Change
  case class UnsupportedChange(range: Range, str: String)                 extends Exception with Change:
    override def getMessage: String = s"Unsupported change in range $range:\n[$str]"

  def apply(range: Range, text: Text): Change =
    val rangeLines = range.getEnd.getLine - range.getStart.getLine

    if text.isEmpty && rangeLines > 0 then
      Remove(range.getStart.getLine, range.getEnd.getLine)
    else if range.getStart == range.getEnd && text.isTerminated then
      Insert(range.getStart.getLine, text)
    else if range.getStart.getLine == range.getEnd.getLine then
      Update(range.getStart.getLine, range.getStart.getCharacter, range.getEnd.getCharacter, text)
    else
      UnsupportedChange(range, text.plainText)

/** Describes lines affected by changing text. */
sealed trait AffectedLines

/** Range of lines in the new text which were inserted. */
case class Inserted(lines: InclusiveRange) extends AffectedLines

/** Range of lines  in the new text which were changed. */
case class Updated(lines: InclusiveRange) extends AffectedLines

/** Range of lines in the old text which were removed */
case class Removed(lines: InclusiveRange) extends AffectedLines
