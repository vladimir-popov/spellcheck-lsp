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
import ru.dokwork.spellchecklsp.TxtDocument._

opaque type Uri = String

object Uri:
  def apply(uri: String): Uri = uri
  def empty: Uri              = ""

extension (uri: Uri) def asString: String = uri

/** The representasion of the document. */
trait Document:

  /** Builds a stream with diagnostics for the document */
  def diagnostics: JStream[Diagnostic]

  /** Returns a stream of possible code actions for the text under cursor `position`. */
  def getTextEdits(position: Position, limit: Int = 10): JStream[TextEdit]

  /** Applies changes and retruns updated document. */
  def applyChange(change: TextDocumentContentChangeEvent): Document

object Document:

  def apply(langId: String, textItem: TextDocumentItem): Document =
    TxtDocument.apply(textItem.getText, Uri(textItem.getUri), AmericanEnglish())

