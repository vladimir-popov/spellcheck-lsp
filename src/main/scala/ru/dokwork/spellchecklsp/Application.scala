package ru.dokwork.spellchecklsp

import com.monovore.decline.{ CommandApp, Opts }
import org.eclipse.lsp4j.launch.LSPLauncher

@main def run(): Unit =
  println("Server has been started")
  val server   = new SpellServer()
  val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
  var client   = launcher.getRemoteProxy()
  server.connect(client)
  launcher.startListening()
  System.err.println("Server has been stopped")
