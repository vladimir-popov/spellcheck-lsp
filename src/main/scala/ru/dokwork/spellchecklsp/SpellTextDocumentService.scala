package ru.dokwork.spellchecklsp

import java.net.URI
import java.util.{ List => JList }
import org.eclipse.lsp4j.jsonrpc.messages.{ Either => JEither }
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.services.TextDocumentService
import org.languagetool.JLanguageTool
import org.languagetool.language.BritishEnglish
import org.languagetool.rules.RuleMatch
import org.languagetool.rules.RuleMatch.Type

import scala.io.Source
import scala.jdk.CollectionConverters.*
import org.eclipse.lsp4j.services.LanguageClient
import java.util.concurrent.ConcurrentHashMap

class SpellTextDocumentService(client: () => LanguageClient) extends TextDocumentService:
  private val langTool = new JLanguageTool(new BritishEnglish())
  private val documents = new ConcurrentHashMap[TextDocumentIdentifier, List[Diagnostic]]()

  /** The Completion request is sent from the client to the server to compute completion items at a
    * given cursor position. Completion items are presented in the IntelliSense user interface. If
    * computing complete completion items is expensive servers can additional provide a handler for
    * the resolve completion item request. This request is sent when a completion item is selected
    * in the user interface.
    *
    * Registration Options: CompletionRegistrationOptions
    */
  override def completion(
      position: CompletionParams
  ): CompletableFuture[JEither[JList[CompletionItem], CompletionList]] = ???

  /** The document open notification is sent from the client to the server to signal newly opened
    * text documents. The document's truth is now managed by the client and the server must not try
    * to read the document's truth using the document's uri.
    *
    * Registration Options: TextDocumentRegistrationOptions
    */
  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    checkDocument(params.getTextDocument().getUri)

  /** The document change notification is sent from the client to the server to signal changes to a
    * text document.
    *
    * Registration Options: TextDocumentChangeRegistrationOptions
    */
  override def didChange(params: DidChangeTextDocumentParams): Unit =
    checkDocument(params.getTextDocument().getUri)

  /** The document close notification is sent from the client to the server when the document got
    * closed in the client. The document's truth now exists where the document's uri points to (e.g.
    * if the document's uri is a file uri the truth now exists on disk).
    *
    * Registration Options: TextDocumentRegistrationOptions
    */
  override def didClose(params: DidCloseTextDocumentParams): Unit = ()

  /** The document save notification is sent from the client to the server when the document for
    * saved in the client.
    *
    * Registration Options: TextDocumentSaveRegistrationOptions
    */
  override def didSave(params: DidSaveTextDocumentParams): Unit = ()

  private def checkDocument(uri: String): Unit =
    val diagnostics: List[Diagnostic] = Source
      .fromFile(new URI(uri))
      .getLines
      .zipWithIndex
      .flatMap { case (line, lineNumber) =>
        langTool.check(line).asScala.map(diagnostic(lineNumber))
      }
      .toList
    client().publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics.asJava))

  private def diagnostic(lineNumber: Int)(rule: RuleMatch): Diagnostic =
    val start    = Position(lineNumber, rule.getFromPos)
    val end      = Position(lineNumber, rule.getToPos)
    val range    = new Range(start, end)
    val severity = rule.getType match
      case Type.UnknownWord => DiagnosticSeverity.Warning
      case Type.Hint        => DiagnosticSeverity.Hint
      case _                => DiagnosticSeverity.Information

    new Diagnostic(range, rule.getMessage, severity, "spellchecklsp")
