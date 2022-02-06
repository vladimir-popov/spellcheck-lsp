package ru.dokwork.spellchecklsp

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.services.{
  LanguageClient,
  LanguageClientAware,
  LanguageServer,
  TextDocumentService,
  WorkspaceService
}

import com.typesafe.scalalogging.StrictLogging

class SpellServer extends LanguageServer with LanguageClientAware with StrictLogging:

  private var client: LanguageClient = _
  private var exitCode: Int          = 1

  /** Captures a client to this server */
  override def connect(client: LanguageClient): Unit =
    this.client = client

  /** The shutdown request is sent from the client to the server. It asks the server to shutdown,
    * but to not exit (otherwise the response might not be delivered correctly to the client). There
    * is a separate exit notification that asks the server to exit.
    */
  override def shutdown(): CompletableFuture[Any] =
    // as a client ask to shutdown, we can close our application with code 0
    CompletableFuture.supplyAsync(() => exitCode = 0)

  /** A notification to ask the server to exit its process. */
  override def exit(): Unit =
    sys.exit(exitCode)

  /** The initialize request is sent as the first request from the client to the server.
    *
    * If the server receives request or notification before the initialize request it should act as
    * follows:
    *   - for a request the respond should be errored with code: -32001. The message can be picked
    *     by the server.
    *   - notifications should be dropped, except for the exit notification. This will allow the
    *     exit a server without an initialize request.
    *
    * Until the server has responded to the initialize request with an InitializeResult the client
    * must not sent any additional requests or notifications to the server.
    *
    * During the initialize request the server is allowed to sent the notifications
    * window/showMessage, window/logMessage and telemetry/event as well as the
    * window/showMessageRequest request to the client.
    */
  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    val capabilities = new ServerCapabilities()
    // server synchronizes entire document
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    capabilities.setCodeActionProvider(true)
    logger.debug(s"Initialize with $capabilities")
    CompletableFuture.supplyAsync(() => InitializeResult(capabilities))

  /** Provides access to the textDocument services. */
  override def getTextDocumentService(): TextDocumentService =
    SpellTextDocumentService(() => client)

  /** Provides access to the workspace services. Stub. */
  override def getWorkspaceService(): WorkspaceService =
    new WorkspaceService():

      /** A notification sent from the client to the server to signal the change of configuration
        * settings.
        */
      def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = ()

      /** The watched files notification is sent from the client to the server when the client
        * detects changes to file watched by the language client.
        */
      def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = ()
