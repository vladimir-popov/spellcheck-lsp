package ru.dokwork.spellchecklsp

import java.util.stream.Stream
import java.util.stream.{ Collectors, Stream => JStream }
import java.util.{ ArrayList, List => JList, Map => JMap }

import scala.jdk.CollectionConverters.*

import org.eclipse.lsp4j.*

import org.languagetool.language.AmericanEnglish
import org.languagetool.markup.{ AnnotatedText, AnnotatedTextBuilder }
import org.languagetool.rules.RuleMatch
import org.languagetool.rules.RuleMatch.Type
import org.languagetool.{ JLanguageTool, Language }
import ru.dokwork.spellchecklsp.TxtDocument._
import com.typesafe.scalalogging.StrictLogging

opaque type Uri = String

object Uri:
  def apply(uri: String): Uri = uri
  def empty: Uri              = ""

extension (uri: Uri) def asString: String = uri

/** The representasion of the document. */
trait Document:

  /** String with URI of this document */
  def uri: Uri

  /** Full text of the document */
  def text: String

  /** Builds a stream with diagnostics for the document */
  def diagnostics: JStream[Diagnostic]

  /** Returns a stream of possible code actions for the text inside the `range`. */
  def getCodeActions(range: Range): JStream[CodeAction]

  /** Applies changes and retruns updated document. */
  def applyChange(change: TextDocumentContentChangeEvent): Document

object Document:

  def apply(langId: String, textItem: TextDocumentItem): Document =
    TxtDocument.create(textItem.getText, Uri(textItem.getUri), AmericanEnglish())

/** One suggested grammar rule.
  *
  * @param range
  *   range inside the document in which the rule can be applied. Always includes only single line.
  */
case class Suggestion(range: Range, rule: RuleMatch):

  def asDiagnostic: Diagnostic =
    val severity = rule.getType match
      case Type.UnknownWord => DiagnosticSeverity.Warning
      case Type.Hint        => DiagnosticSeverity.Hint
      case _                => DiagnosticSeverity.Information
    Diagnostic(range, rule.getMessage, severity, "spellchecklsp")

  def asTextChanges: JStream[TextEdit] =
    rule.getSuggestedReplacements.stream.map(TextEdit(range, _))

/** Implementation of the [[Document]] for plain text */
class TxtDocument(
    val uri: Uri,
    language: Language,
    lines: IndexedSeq[String],
    suggestions: Map[Int, IndexedSeq[Suggestion]]
) extends Document
    with StrictLogging:

  def text: String = joinLines(lines)

  def diagnostics =
    suggestions.view.values.flatten.stream.map(_.asDiagnostic)

  def getCodeActions(range: Range): JStream[CodeAction] =
    suggestions.view.values.flatten.stream.flatMap(_.asTextChanges).map(_.asAction(uri))

  def applyChange(change: TextDocumentContentChangeEvent) =
    val (start, end) = change.getRange.getStart -> change.getRange.getEnd
    val newLines     =
      if (start.getLine == end.getLine)
        applyOneLineChange(change.getText, start.getLine, start.getCharacter, end.getCharacter)(
          lines
        )
      else
        applyMultiLinesChange(change.getText, start, end)(lines)

    val updatedLines = newLines.slice(start.getLine, end.getLine + 1)

    val newSuggestions =
      if (updatedLines.isEmpty)
        suggestions.takeWhile((i, _) => i < start.getLine)
      else
        merge(suggestions, check(updatedLines, language, start.getLine))

    logger.trace(
      s"""New change:
          |--------------
          |$change
          |--------------
          |Updated lines:
          |--------------
          |${updatedLines.mkString("\n")}
          |--------------
          |Actual suggestions:
          |--------------
          |${newSuggestions.mkString("\n")}
          |--------------""".stripMargin
    )

    TxtDocument(uri, language, newLines, newSuggestions)

  private def applyOneLineChange(
      newText: String,
      line: Int,
      from: Int,
      to: Int
  ): IndexedSeq[String] => IndexedSeq[String] = lines =>
    if (line >= lines.length) lines :+ newText
    else {
      val str    = lines(line)
      val prefix = str.slice(0, from)
      val suffix = str.slice(to, str.length)
      lines.updated(line, prefix ++ newText ++ suffix)
    }

  private def applyMultiLinesChange(
      newText: String,
      start: Position,
      end: Position
  ): IndexedSeq[String] => IndexedSeq[String] = lines =>
    if (lines.length <= start.getLine) lines ++ splitLines(newText)
    else {
      val newLines     = splitLines(newText).zipWithIndex
        .map((str, i) => str -> (i + start.getLine))
      val croppedLines = lines.slice(0, start.getLine + newLines.length)
      newLines.foldLeft(croppedLines) {
        case (lines, (str, i)) if i == start.getLine =>
          applyOneLineChange(str, i, start.getCharacter, lines(i).length)(lines)

        case (lines, (str, i)) if i == end.getLine =>
          applyOneLineChange(str, i, 0, end.getCharacter)(lines)

        case (lines, (str, i)) =>
          applyOneLineChange(str, i, 0, lines(i).length)(lines)
      }
    }

  private def merge(
      current: Map[Int, IndexedSeq[Suggestion]],
      changed: Map[Int, IndexedSeq[Suggestion]]
  ): Map[Int, IndexedSeq[Suggestion]] =
    current ++ changed

object TxtDocument:

  def create(
      text: String,
      uri: Uri = Uri.empty,
      language: Language = AmericanEnglish()
  ): TxtDocument =
    val lines       = splitLines(text)
    val suggestions = check(lines, language)
    TxtDocument(uri, language, lines, suggestions)

  def check(
      lines: IndexedSeq[String],
      language: Language,
      shift: Int = 0
  ): Map[Int, IndexedSeq[Suggestion]] =
    val langTool = JLanguageTool(language)
    langTool.disableRule("UPPERCASE_SENTENCE_START")
    lines.zipWithIndex
      .map((text, line) =>
        (line + shift) -> langTool
          .check(text)
          .stream
          .map(rule =>
            Suggestion(
              Range(Position(line + shift, rule.getFromPos), Position(line + shift, rule.getToPos)),
              rule
            )
          )
          .toIndexedSeq
      )
      .toMap

  @inline
  def splitLines(str: String): IndexedSeq[String] =
    if (str.isEmpty) IndexedSeq.empty
    else str.split("\\R", -1).toIndexedSeq

  @inline
  def joinLines(lines: IndexedSeq[String], sep: String = "\n"): String =
    lines.mkString(sep)
