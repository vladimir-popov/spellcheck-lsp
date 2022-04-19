package ru.dokwork.spellchecklsp

import scala.annotation.tailrec
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.Position
import scala.Range.{ Exclusive => ExclusiveRange, Inclusive => InclusiveRange }
import com.typesafe.scalalogging.StrictLogging

/** The first character and the length of a substring from the text */
case class Substring(char: Int, length: Int):
  /** Returns substring from the argument. */
  def apply(str: String): String = str.slice(char, char + length)

/** Syntax to create the [[Text]] from a string. */
extension (str: String)

  def toText: Text =
    @tailrec
    def loop(line: Int, i: Int, index: Map[Int, Substring]): Map[Int, Substring] =
      if i >= str.length then
        index
      else
        val Substring(j, ln) = nextEOL(i)
        loop(line + 1, j + ln, index + (line -> Substring(i, j - i)))

    Text(str, loop(0, 0, Map.empty))

  @tailrec
  private[spellchecklsp] def nextEOL(i: Int): Substring =
    if i >= str.length then
      Substring(str.length, 0)
    else if str.charAt(i) == '\n' then
      Substring(i, 1)
    else if str.charAt(i) == '\r' then
      val ln = if i < (str.length - 1) && str.charAt(i + 1) == '\n' then 2 else 1
      Substring(i, ln)
    else
      nextEOL(i + 1)

/** Wrapper around a string provids operations over lines of this string. The string is sliced to
  * lines with one of possible end of line: '\n' | '\r' | '\r\n'.
  *
  * @param index
  *   zero-based dictionary of the lines where every line is the [[Substring]] from the first
  *   zero-based symbol til EOL (exclusive) or end of the text.
  */
class Text private[spellchecklsp] (val plainText: String, index: Map[Int, Substring]):

  override def toString = plainText

  def isEmpty = plainText.isEmpty

  def nonEmpty = !isEmpty

  /** Returns the first met one of the ends of line: '\n' | '\r' | '\r\n', or an empty string if no
    * one was found.
    */
  lazy val eol: String =
    plainText.nextEOL(0).apply(plainText)

  /** Returns true if the last symbol in this text is EOL. false for empty text.
    */
  def isTerminated: Boolean =
    if isEmpty then
      false
    else
      val last = plainText.last
      last == '\n' || last == '\r'

  /** Returns line with number `i` if it exists, or None. The returned line dowsn't containn EOL
    * symbol.
    *
    * @param i
    *   zero-based number of line.
    */
  def line(i: Int): Option[String] =
    index.get(i).map(_.apply(plainText))

  /** Iterator over lines of this text. */
  def lines: Iterator[String] =
    (0 until linesCount).iterator.flatMap(line)

  /** Returns count of lines in this text. */
  def linesCount: Int = index.size

  /** Returns a substring in the specified range excluding the `end` position. */
  def substring(range: Range): String =
    val maybeString =
      for
        from <- indexOf(range.getStart)
        to    = indexOf(range.getEnd).getOrElse(plainText.length)
      yield plainText.slice(from, to)

    maybeString.getOrElse("")

  /** Returns an index of the character from the `plainText` in the position `pos`. */
  private def indexOf(pos: Position): Option[Int] = index.get(pos.getLine).map(
    _.char + pos.getCharacter
  )

  /** Changes this text in the specified `range` with the `newText`.
    * @see
    *   [[Change]]
    * @return
    *   updated text with affected lines (new or old text depends on changes).
    */
  def change(range: Range, newText: String): (Text, AffectedLines) =
    apply(Change(range, newText.toText))

  private def apply: Change => (Text, AffectedLines) =
    case Change.InsertText(line, newText)              =>
      insert(line, newText) -> Inserted(line to (line + newText.linesCount - 1))
    case Change.RemoveLines(from, until)               =>
      remove(from, until) -> Removed(from to (until - 1))
    case Change.ReplaceInLine(line, from, until, text) =>
      val lns = 0 max (text.linesCount - 1)
      update(line, from, until, text) -> Updated(line to (line + lns))
    case unsupported: Change.UnsupportedChange         =>
      throw unsupported

  private def insert(line: Int, newText: Text): Text =
    val str =
      index.get(line) match
        case Some(Substring(index, _)) =>
          plainText.slice(0, index) + newText.plainText + plainText.slice(
            index,
            plainText.length
          )
        case None                      =>
          val sep =
            if isTerminated then ""
            else if newText.isTerminated then newText.eol
            // FIXME I'm not sure about EOL in such case
            else System.lineSeparator

          plainText + sep + newText.plainText

    str.toText

  private def update(line: Int, fromChar: Int, untilChar: Int, text: Text): Text =
    val from  = index(line).char + fromChar
    val until = index(line).char + untilChar
    (plainText.slice(0, from) + text.toString + plainText.slice(until, plainText.length)).toText

  private def remove(fromLine: Int, toLine: Int): Text =
    val from = index(fromLine).char
    val to   = index(toLine).char
    (plainText.slice(0, from) + plainText.slice(to, plainText.length)).toText

  private sealed trait Change

  private object Change:
    case class InsertText(line: Int, newText: Text)                                   extends Change
    case class RemoveLines(fromLine: Int, untilLine: Int)                             extends Change
    case class ReplaceInLine(line: Int, fromChar: Int, untilChar: Int, newText: Text) extends Change
    case class UnsupportedChange(range: Range, str: String)                           extends Exception with Change:
      override def getMessage: String = s"Unsupported change in range $range:\n[$str]"

    def apply(range: Range, newText: Text): Change =
      val rangeLines = range.getEnd.getLine - range.getStart.getLine

      if newText.isEmpty && rangeLines > 0 then
        RemoveLines(range.getStart.getLine, range.getEnd.getLine)
      else if range.getStart == range.getEnd && newText.isTerminated then
        InsertText(range.getStart.getLine, newText)
      else if range.getStart.getLine == range.getEnd.getLine then
        ReplaceInLine(
          range.getStart.getLine,
          range.getStart.getCharacter,
          range.getEnd.getCharacter,
          newText
        )
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
