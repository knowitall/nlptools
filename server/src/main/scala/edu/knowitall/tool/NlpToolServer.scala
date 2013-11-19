package edu.knowitall.tool

import java.io.File
import dispatch._
import Defaults._

object NlpToolServer extends App {
  case class Config(configFile: File = null, logPath: Option[File] = None, port: Int = 8080)

  val parser = new scopt.OptionParser[Config]("scopt") {
    arg[File]("<file>") action { (x, c) =>
      c.copy(configFile = x)
    }
    opt[File]('l', "logs") action { (x, c) =>
      c.copy(logPath = Some(x))
    }
    opt[Int]('p', "port") action { (x, c) =>
      c.copy(port = x)
    }
  }

  parser.parse(args, Config()).foreach { config =>
    val server = new NlpToolServer(config, readTools(config.configFile))
    server.run()
  }

  def readTools(configFile: File): Seq[Tool] = {
    val source = io.Source.fromFile(configFile)
    try {
      source.getLines().map { line =>
        val Array(path, url, jar) = line.split("\t")
        Tool(path, url)
      }.toList
    }
    finally {
      source.close()
    }
  }
}

case class Tool(path: String, url: String) {
  val svc = dispatch.url(url)

  def withoutTrailingSlash = {
    if (path endsWith "/") this.copy(path = path.dropRight(1))
    else this
  }
}
class NlpToolServer(config: NlpToolServer.Config, tools: Seq[Tool]) {
  val toolMap = tools.map(_.withoutTrailingSlash).groupBy(_.path).map { case (k, v) =>
    (k, v.head)
  }

  import unfiltered.request._
  import unfiltered.response._
  import unfiltered.filter.Planify

  def run() {
    def dropSlash(s: String) = if (s endsWith "/") s.dropRight(1) else s
    val plan = Planify {
      // GET status as HTML
      case GET(Path("/")) =>
        ResponseString(
          """<html><head><title>Http NlpTools Status</title></head><body>""" +
          tools.map { tool =>
            val path = dropSlash(tool.path)
            val color = Http(tool.svc OK as.String).either() match {
              case Left(_) => "darkred"
              case Right(_) => "darkgreen"
            }
            s"<font color='$color'>$path</font> at <a href='${tool.url}'>${tool.url}</a>"
          }.mkString("<br/>\n") +
          """</html>"""
        )

      // GET status as plain text
      case GET(Path("/plaintext")) =>
        ResponseString(tools.map(_.path).mkString("\n"))

      case GET(Path(path)) if toolMap.contains(dropSlash(path)) =>
        val tool = toolMap(dropSlash(path))
        ResponseString(Http(tool.svc OK as.String).apply())

      // GET tool logs
      case GET(Path(Seg(tool :: impl :: ext :: Nil))) 
        if toolMap.contains(s"/$tool/$impl") && 
           config.logPath.isDefined =>
          val filename = s"$tool-$impl.$ext"
          val file = new File(config.logPath.get, filename)
          if (file.exists()) {
            ResponseString(io.Source.fromFile(file).getLines.mkString("\n"))
          }
          else {
            ResponseString("File does not exists: " + file)
          }
      case req @ POST(Path(path)) if toolMap.contains(dropSlash(path)) =>
        val tool = toolMap(dropSlash(path))
        val payload = Body.string(req)
        ResponseString(Http(tool.svc << payload OK as.String).apply())
      case req @ GET(Path(path)) => ResponseString("Tool not found in GET: " + path)
      case req @ POST(Path(path)) => ResponseString("Tool not found in POST: " + path)
    }

    unfiltered.jetty.Http(config.port).filter(plan).run()
    System.out.println("Server started on port: " + config.port)
  }
}
