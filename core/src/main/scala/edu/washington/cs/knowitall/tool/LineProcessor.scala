package edu.washington.cs.knowitall.tool

import edu.washington.cs.knowitall.common.Timing
import java.io.File
import java.util.Scanner
import java.io.PrintWriter

abstract class LineProcessor(name: String) {
  import scopt.immutable._
  import scopt.immutable.OptionParser._

  case class Config(val server: Boolean = false, val port: Int = 8080, val outputFile: Option[File] = None, val inputFile: Option[File] = None)

  val parser = new scopt.immutable.OptionParser[Config](name) {
    def options = Seq(
      flag("server", "run as a server") { (c: Config) => c.copy(server = true) },
      intOpt("port", "which port to run the server on") { (port: Int, c: Config) => c.copy(port = port) },
      argOpt("input", "file to input from") { (path: String, c: Config) => c.copy(inputFile=Some(new File(path))) },
      argOpt("output", "file to output to") { (path: String, c: Config) => c.copy(outputFile=Some(new File(path))) }
    )
  }

  def main(args: Array[String]) = {
    parser.parse(args, new Config) match {
      case Some(config) => run(config)
      case None =>
    }
  }

  def init(config: Config): Unit = {}

  def run(config: Config) {
    init(config)
    if (config.server) new LineProcessorServer(this.getClass.getSimpleName(), config.port, process)
    else runCli(config)
  }

  def handle(writer: PrintWriter, line: String): Unit = {
    writer.println(process(line))
    writer.flush()
  }

  def process(line: String): String

  def runCli(config: Config) {
    val scanner = config.inputFile match {
      case Some(file) => new Scanner(file, "UTF-8")
      case None => new Scanner(System.in, "UTF-8")
    }

    val writer = config.outputFile match {
      case Some(file) => new PrintWriter(file, "UTF-8")
      case None => new PrintWriter(System.out)
    }

    val ns = Timing.time {
      while (scanner.hasNextLine) {
        handle(writer, scanner.nextLine)
      }
    }

    System.err.println(Timing.Seconds.format(ns))

    scanner.close()
    writer.close()
  }
}

// This is a separate class so that optional dependencies are not loaded
// unless a server instance is being create.
class LineProcessorServer(name: String, port: Int, process: String => String) {
  import unfiltered.request._
  import unfiltered.response._
  import unfiltered.filter.Planify

  def run {
    val plan = Planify {
      case Path(Seg(p :: Nil)) => ResponseString(p)
      case req @ PUT(_) => ResponseString(process(Body.string(req)))
      case req @ GET(_) => ResponseString("Post a line to process for: " + name)
    }

    unfiltered.jetty.Http(port).filter(plan).run()
  }
}