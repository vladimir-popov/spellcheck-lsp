package ru.dokwork.spellchecklsp

import scala.annotation.tailrec
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.Position
import scala.Range.{ Exclusive => ExclusiveRange, Inclusive => InclusiveRange }
import com.typesafe.scalalogging.StrictLogging

/** Syntax to create [[Text]] from a string. */
extension (str: String)
  def toText: Text =
    val EOL = System.lineSeparator

    @tailrec
    def loop(line: Int, character: Int, lines: Map[Int, Int]): Map[Int, Int] =
      if character >= 0 && character < str.length && !lines.contains(character) then
        loop(line + 1, str.indexOf(EOL, character) + 1, lines + (line -> character))
      else
        lines

    Text(str)(loop(0, 0, Map.empty))

/** Wrapper around a string providing operations over lines of text. Text is sliced to lines with
  * [[System.lineSeparator system line separator]].
  *
  * @param index
  *   zero-based dictionary of the first zero-based characters of lines.
  */
case class Text private[spellchecklsp] (plainText: String)(index: Map[Int, Int]) extends StrictLogging:

  override def toString = plainText

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
    val maybeFrom = index.get(i)
    val to        = index.getOrElse(i + 1, plainText.length)
    maybeFrom.map(from => plainText.slice(from, to))

  /** Iterator over lines of this text. */
  def lines: Iterator[String] =
    (0 until linesCount).iterator.flatMap(line)

  /** Returns count of lines in this text. */
  def linesCount: Int = index.size

  /** Returns a substring in the specified range excluding the `end` position.
    */
  def substring(range: Range): String =
    val maybeString =
      for
        from <- indexOf(range.getStart)
        to    = indexOf(range.getEnd).getOrElse(plainText.length)
      yield plainText.slice(from, to)

    maybeString.getOrElse("")

  /** Changes this text in the specified `range` with the `newText`.
    * @see
    *   [[Change]]
    * @return
    *   updated text with affected lines (new or old text depends on changes).
    */
  def change(range: Range, newText: String): (Text, AffectedLines) =
    val ch = Change(range, newText.toText)
    logger.trace(s"Applying $ch")
    apply(ch)

  private def apply: Change => (Text, AffectedLines) =
    case Change.Insert(line, text)              =>
      insert(line, text) -> Inserted(line to (line + text.linesCount - 1))
    case Change.Remove(from, until)             =>
      remove(from, until) -> Removed(from to (until - 1))
    case Change.Update(line, from, until, text) =>
      val lns = 0 max (text.linesCount - 1)
      update(line, from, until, text) -> Updated(line to (line + lns))
    case unsupported: Change.UnsupportedChange  =>
      throw unsupported

  private def insert(line: Int, newText: Text): Text =
    val str =
      index.get(line) match
        case Some(index) =>
          plainText.slice(0, index) + newText.plainText + plainText.slice(index, plainText.length)
        case None        =>
          plainText + EOL + newText.plainText
    str.toText

  private def update(line: Int, fromChar: Int, untilChar: Int, text: Text): Text =
    val from  = index(line) + fromChar
    val until = index(line) + untilChar
    (plainText.slice(0, from) + text.toString + plainText.slice(until, plainText.length)).toText

  private def remove(fromLine: Int, toLine: Int): Text =
    val from = index(fromLine)
    val to   = index(toLine)
    (plainText.slice(0, from) + plainText.slice(to, plainText.length)).toText

  private def indexOf(pos: Position): Option[Int] = index.get(pos.getLine).map(
    _ + pos.getCharacter
  )

  private sealed trait Change

  private object Change:
    case class Insert(line: Int, newText: Text)                             extends Change
    case class Remove(fromLine: Int, untilLine: Int)                        extends Change
    case class Update(line: Int, fromChar: Int, untilChar: Int, text: Text) extends Change
    case class UnsupportedChange(range: Range, str: String)                 extends Exception with Change:
      override def getMessage: String = s"Unsupported change in range $range:\n[$str]"

    def apply(range: Range, newText: Text): Change =
      val rangeLines = range.getEnd.getLine - range.getStart.getLine

      if newText.isEmpty && rangeLines > 0 then
        Remove(range.getStart.getLine, range.getEnd.getLine)
      else if range.getStart == range.getEnd && newText.isTerminated then
        Insert(range.getStart.getLine, newText)
      else if range.getStart.getLine == range.getEnd.getLine then
        Update(range.getStart.getLine, range.getStart.getCharacter, range.getEnd.getCharacter, newText)
      else
        UnsupportedChange(range, newText.plainText)

/** Describes lines affected by changing text. */
sealed trait AffectedLines:
  def lines: InclusiveRange

/** Range of lines in the new text which were inserted. */
case class Inserted(lines: InclusiveRange) extends AffectedLines

/** Range of lines  in the new text which were changed. */
case class Updated(lines: InclusiveRange) extends AffectedLines

/** Range of lines in the old text which were removed */
case class Removed(lines: InclusiveRange) extends AffectedLines
