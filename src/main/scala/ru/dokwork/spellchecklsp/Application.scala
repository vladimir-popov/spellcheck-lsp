package ru.dokwork.spellchecklsp

import org.eclipse.lsp4j.launch.LSPLauncher

import org.slf4j.{ Logger, LoggerFactory }

val logger = LoggerFactory.getLogger("spellcheck-lsp")

@main def run(): Unit =
  logger.info("Server has been started")

  val server   = SpellServer()
  val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
  var client   = launcher.getRemoteProxy()
  server.connect(client)
  launcher.startListening().get()

  logger.info("Server has been stopped")
