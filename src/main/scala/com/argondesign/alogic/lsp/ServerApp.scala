////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Language server entry point
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lsp

import org.eclipse.lsp4j.launch.LSPLauncher

import java.net.ServerSocket

object ServerApp {

  def main(args: Array[String]): Unit = {
    val serverSocket = new ServerSocket(0)
    println(s"${serverSocket.getLocalPort()}")
    val socket = serverSocket.accept()

    val inputStream = socket.getInputStream()
    val outputStream = socket.getOutputStream()

    val server = new AlogicLanguageServer();
    val launcher = LSPLauncher.createServerLauncher(server, inputStream, outputStream)

    val client = launcher.getRemoteProxy()
    server.connect(client)

    launcher.startListening()
  }

}
