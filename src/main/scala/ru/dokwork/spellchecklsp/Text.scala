package ru.dokwork.spellchecklsp

import scala.annotation.tailrec
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.Position
import scala.Range.{ Inclusive => InclusiveRange }

case class Text(plainText: String)(lines: Map[Int, Int]):

  /** Returns line with number `i` if it exists, or empty string.
    * @param i
    *   zero-based number of line.
    */
  def line(i: Int): String =
    val maybeFrom = lines.get(i)
    val to        = lines.getOrElse(i + 1, plainText.length)
    maybeFrom.map(from => plainText.slice(from, to)).getOrElse("")

  def substring(range: Range): String =
    val from = indexOf(range.getStart)
    val to   = indexOf(range.getEnd)
    plainText.slice(from, to)

  /** Insert text to the text in specified range. Returns new text and range of updated lines in the
    * new text.
    */
  def change(range: Range, newText: String): (Text, InclusiveRange) =
    val from          = indexOf(range.getStart)
    val to            = indexOf(range.getEnd)
    val patchedString = plainText.slice(0, from) + newText + plainText.slice(to, plainText.length)
    // todo: avoid recreating full text here
    patchedString.asText -> (range.getStart.getLine to range.getEnd.getLine)

  private def indexOf(pos: Position): Int =
    lines.get(pos.getLine).map(_ + pos.getCharacter).getOrElse(plainText.length)

extension (str: String)
  def asText: Text =
    @tailrec
    def loop(line: Int, character: Int, lines: Map[Int, Int]): Map[Int, Int] =
      if character >= 0 && character < str.length && !lines.contains(character) then
        Thread.`yield`
        loop(line + 1, str.indexOf("\n", character) + 1, lines + (line -> character))
      else lines

    Text(str)(loop(0, 0, Map.empty))
