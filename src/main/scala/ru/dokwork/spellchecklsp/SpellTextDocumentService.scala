package ru.dokwork.spellchecklsp

import java.net.URI
import java.nio.file.{ Files, Paths }
import java.util.concurrent.{ CompletableFuture, ConcurrentHashMap }
import java.util.stream.{ Collectors, Stream => JStream }
import java.util.{ Collections, List => JList, Map => JMap }

import scala.jdk.CollectionConverters.*

import scala.jdk.CollectionConverters.*

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.jsonrpc.messages.{ Either => JEither }
import org.eclipse.lsp4j.services.{ LanguageClient, TextDocumentService }

import com.google.common.collect.{ Lists, Streams }
import com.typesafe.scalalogging.StrictLogging
import org.languagetool.JLanguageTool
import org.languagetool.language.{ AmericanEnglish, BritishEnglish }
import org.languagetool.rules.RuleMatch

class SpellTextDocumentService(client: () => LanguageClient)
    extends TextDocumentService
    with StrictLogging:
  self =>

  // two different T are equal when have the same version
  private case class Versioned[T](version: Int)(val value: T)

  private val documents = ConcurrentHashMap[Uri, Versioned[Document]]()

  /** The code action request is sent from the client to the server to compute commands for a given
    * text document and range. These commands are typically code fixes to either fix problems or to
    * beautify/refactor code.
    *
    * Registration Options: TextDocumentRegistrationOptions
    */
  override def codeAction(
      params: CodeActionParams
  ): CompletableFuture[JList[JEither[Command, CodeAction]]] =
    logger.trace(s"Code action $params")
    CompletableFutures.computeAsync { _ =>
      val uri = Uri(params.getTextDocument.getUri)
      val actions = for
        verDoc <- documents.stream(uri)
        edit <- verDoc.value.getTextEdits(params.getRange.getStart)
      yield JEither.forRight[Command, CodeAction](edit.asAction(uri))
      actions.toJList
    }

  /** The document open notification is sent from the client to the server to signal newly opened
    * text documents. The document's truth is now managed by the client and the server must not try
    * to read the document's truth using the document's uri.
    *
    * Registration Options: TextDocumentRegistrationOptions
    */
  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    val textDocument = params.getTextDocument
    val document     = Document(params.getTextDocument.getLanguageId, textDocument)
    val uri          = Uri(textDocument.getUri)
    documents.put(
      uri,
      Versioned(textDocument.getVersion)(document)
    )
    publishDiagnostics(uri, document)

  /** The document change notification is sent from the client to the server to signal changes to a
    * text document.
    *
    * Registration Options: TextDocumentChangeRegistrationOptions
    */
  override def didChange(params: DidChangeTextDocumentParams): Unit =
    logger.trace(s"Did change:\n$params")

    def updateDoc(doc: Versioned[Document]): Versioned[Document] =
      logger.debug(
        s"${params.getTextDocument.getUri}. New changes with version ${params.getTextDocument.getVersion}. Current document version is ${doc.version}"
      )
      val updatedDoc =
        params.getContentChanges.asScala.foldLeft(doc.value)(_ applyChange _)
      Versioned(params.getTextDocument.getVersion)(updatedDoc)

    val uri    = Uri(params.getTextDocument.getUri)
    val verDoc = documents.computeIfPresent(
      uri,
      (_, verDoc) => updateDoc(verDoc)
    )
    publishDiagnostics(uri, verDoc.value)

  /** The document close notification is sent from the client to the server when the document got
    * closed in the client. The document's truth now exists where the document's uri points to (e.g.
    * if the document's uri is a file uri the truth now exists on disk).
    *
    * Registration Options: TextDocumentRegistrationOptions
    */
  override def didClose(params: DidCloseTextDocumentParams): Unit =
    logger.trace(s"Did close\n$params")
    documents.remove(params.getTextDocument.getUri)

  /** The document save notification is sent from the client to the server when the document for
    * saved in the client.
    *
    * Registration Options: TextDocumentSaveRegistrationOptions
    */
  override def didSave(params: DidSaveTextDocumentParams): Unit =
    logger.trace(s"Did save\n$params")

  private def publishDiagnostics(uri: Uri, document: Document): Unit =
    val diagnostics = document.diagnostics.toJList
    logger.debug(s"${diagnostics.size} diagnostics exist for $uri")
    client().publishDiagnostics(
      PublishDiagnosticsParams(uri.asString, diagnostics)
    )
