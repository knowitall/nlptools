package edu.knowitall.tool

import java.io.File
import dispatch._
import Defaults._

object NlpToolServer extends App {
  case class Config(configFile: File = null, port: Int = 8080)

  val parser = new scopt.OptionParser[Config]("scopt") {
    arg[File]("<file>") action { (x, c) =>
      c.copy(configFile = x)
    }
    opt[Int]('p', "port") action { (x, c) =>
      c.copy(port = x)
    }
  }

  parser.parse(args, Config()).foreach { config =>
    val server = new NlpToolServer(config.port, readTools(config.configFile))
    server.run()
  }

  def readTools(configFile: File): Seq[Tool] = {
    val source = io.Source.fromFile(configFile)
    try {
      source.getLines().map { line =>
        val Array(path, port, jar) = line.split("\t")
        Tool(path, port.toInt)
      }.toList
    }
    finally {
      source.close()
    }
  }
}

case class Tool(path: String, port: Int) {
  val svc = host("localhost", port)
}
class NlpToolServer(port: Int, tools: Seq[Tool]) {
  val toolMap = tools.groupBy(_.path).map { case (k, v) => (k, v.head) }

  import unfiltered.request._
  import unfiltered.response._
  import unfiltered.filter.Planify

  def run() {
    val plan = Planify {
      case GET(Path("/")) => ResponseString(tools.map(_.path).mkString("\n"))
      case GET(Path(path)) if toolMap.contains(path) =>
        val tool = toolMap(path)
        ResponseString(Http(tool.svc OK as.String).apply())
      case req @ POST(Path(path)) if toolMap.contains(path) =>
        val tool = toolMap(path)
        val payload = Body.string(req)
        ResponseString(Http(tool.svc << payload OK as.String).apply())
      case req @ GET(Path(path)) => ResponseString("Tool not found in GET: " + path)
      case req @ POST(Path(path)) => ResponseString("Tool not found in POST: " + path)
    }

    unfiltered.jetty.Http(port).filter(plan).run()
    System.out.println("Server started on port: " + port);
  }
}
