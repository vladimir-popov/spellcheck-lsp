package ru.dokwork.spellchecklsp

import java.util.stream.{ Collectors, Stream => JStream }
import java.util.{ ArrayList, List => JList, Map => JMap }

import scala.jdk.CollectionConverters.*

import org.eclipse.lsp4j.*

import com.typesafe.scalalogging.StrictLogging
import org.languagetool.language.AmericanEnglish
import org.languagetool.markup.{ AnnotatedText, AnnotatedTextBuilder }
import org.languagetool.rules.RuleMatch
import org.languagetool.rules.RuleMatch.Type
import org.languagetool.{ JLanguageTool, Language }

/** Implementation of the [[Document]] for a plain text.
  *
  * @param text
  *   the text in this document.
  * @param language
  *   of this document.
  * @param suggestions
  *   dictionary of suggestions per line of the text.
  */
class TxtDocument private (
    val text: Text,
    language: Language,
    suggestions: Map[Int, IndexedSeq[Suggestion]]
) extends Document
    with StrictLogging:

  def diagnostics =
    suggestions.view.values.flatten.stream.map(_.asDiagnostic)

  def getTextEdits(position: Position, limit: Int = 10): JStream[TextEdit] =
    for
      sgs  <- suggestions.stream(position.getLine)
      sg   <- sgs.stream.filter(_.range.contains(position))
      edit <- sg.asTextEdits.limit(limit)
    yield edit

  def applyChange(change: TextDocumentContentChangeEvent): TxtDocument =
    val (updatedText, affectedLines) = text.change(change.getRange, change.getText)
    affectedLines match
      case Removed(lines) =>
        new TxtDocument(updatedText, language, suggestions -- lines)

      case affected @ (_: Updated | _: Inserted) =>
        val range              = Range(Position(affected.lines.head, 0), Position(affected.lines.last + 1, 0))
        val substring          = updatedText.substring(range)
        val updatedSuggestions = TxtDocument.check(substring.toText, language, affected.lines.head)
        new TxtDocument(updatedText, language, suggestions ++ updatedSuggestions)

object TxtDocument:

  def apply(
      text: String,
      uri: Uri = Uri.empty,
      language: Language = AmericanEnglish()
  ): TxtDocument =
    val txt         = text.toText
    val suggestions = check(txt, language)
    new TxtDocument(txt, language, suggestions)

  /** Checks a text for mistakes.
    *
    * @param text
    *   which should be shecked.
    * @param language
    *   language of the text.
    * @param shift
    *   count of additional symbols before [[RuleMatch]]s.
    *
    * @return
    *   dictionary of suggestions per line of the text.
    */
  private[spellchecklsp] def check(
      text: Text,
      language: Language,
      shift: Int = 0
  ): Map[Int, IndexedSeq[Suggestion]] =
    val langTool = JLanguageTool(language)
    text.lines.zipWithIndex
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
