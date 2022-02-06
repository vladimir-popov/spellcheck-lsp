package ru.dokwork.spellchecklsp

import org.eclipse.lsp4j.launch.LSPLauncher
import com.typesafe.scalalogging.Logger

@main def run(): Unit =
  val logger = Logger("ru.dokwork.spellchecklsp.Application")

  logger.info("\n" + ("*" * 30) + "\n* Server has been started\n" + ("*" * 30))

  val server   = SpellServer()
  val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
  var client   = launcher.getRemoteProxy()
  server.connect(client)
  launcher.startListening().get()

  logger.info("Server has been stopped")
