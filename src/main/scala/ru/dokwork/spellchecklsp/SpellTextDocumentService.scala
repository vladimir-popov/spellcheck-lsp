package ru.dokwork.spellchecklsp

import java.net.URI
import java.nio.file.{ Files, Paths }
import java.util.concurrent.{ CompletableFuture, ConcurrentHashMap }
import java.util.stream.{ Collectors, Stream }
import java.util.{ Optional, _ }

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.{ LanguageClient, TextDocumentService }

import com.google.common.collect.{ Lists, Streams }
import org.languagetool.JLanguageTool
import org.languagetool.language.BritishEnglish
import org.slf4j.LoggerFactory

class SpellTextDocumentService(client: () => LanguageClient) extends TextDocumentService:
  private val logger = LoggerFactory.getLogger(getClass)

  private type Uri         = String
  private type Suggestions = Map[Range, LineRuleMatch]

  private val documents = ConcurrentHashMap[Uri, Suggestions]()
  private val langTool  = JLanguageTool(new BritishEnglish())

  /** The code action request is sent from the client to the server to compute commands for a given
    * text document and range. These commands are typically code fixes to either fix problems or to
    * beautify/refactor code.
    *
    * Registration Options: TextDocumentRegistrationOptions
    */
  override def codeAction(
      params: CodeActionParams
  ): CompletableFuture[List[Either[Command, CodeAction]]] =
    CompletableFutures.computeAsync { _ =>
      val suggestions: Suggestions =
        documents.getOrDefault(params.getTextDocument.getUri, Collections.emptyMap)

      val actions = suggestions.findValues(_.contains(params.getRange)).flatMap {
        _.asTextChanges.map(_.asAction(params.getTextDocument.getUri))
      }

      actions.map(Either.forRight[Command, CodeAction]).toList
    }

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
  override def didClose(params: DidCloseTextDocumentParams): Unit =
    documents.remove(params.getTextDocument.getUri)

  /** The document save notification is sent from the client to the server when the document for
    * saved in the client.
    *
    * Registration Options: TextDocumentSaveRegistrationOptions
    */
  override def didSave(params: DidSaveTextDocumentParams): Unit = ()

  private def checkDocument(uri: Uri): Unit =
    val suggestions: Suggestions = Files
      .lines(Paths.get(URI(uri)))
      .zipWithIndex
      .flatMap { case (line, lineNumber) =>
        langTool
          .check(line)
          .stream
          .map(LineRuleMatch(_, lineNumber))
          .map(rule => rule.getRange -> rule)
      }
      .toMap

    documents.put(uri, suggestions)

    client().publishDiagnostics(
      PublishDiagnosticsParams(uri, suggestions.values.stream.map(_.asDiagnostic).toList)
    )
