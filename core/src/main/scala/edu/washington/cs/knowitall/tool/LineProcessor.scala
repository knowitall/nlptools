package edu.washington.cs.knowitall.tool

import unfiltered.request._
import unfiltered.response._
import unfiltered.filter.Planify
import edu.washington.cs.knowitall.common.Timing

abstract class LineProcessor(name: String) {
  import scopt.immutable._
  import scopt.immutable.OptionParser._

  class Config(val server: Boolean = false, val port: Int = 8080) {
    def copy(server: Boolean = server, port: Int = port) =
      new Config(server, port)
  }

  val parser = new scopt.immutable.OptionParser[Config](name) {
    def options = Seq(
      flag("server", "run as a server") { (c: Config) => c.copy(server = true) },
      intOpt("port", "which port to run the server on") { (port: Int, c: Config) => c.copy(port = port) }
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
    if (config.server) runServer(config)
    else runCli(config)
  }

  def handle(line: String): Unit = {
    println(process(line))
  }

  def process(line: String): String

  def runCli(config: Config) {
    val scanner = new java.util.Scanner(System.in, "UTF-8")

    val ns = Timing.time {
      while (scanner.hasNextLine) {
        handle(scanner.nextLine)
      }
    }

    System.err.println(Timing.Seconds.format(ns))
  }

  def runServer(config: Config) = {
    val port = config.port

    val plan = Planify {
      case Path(Seg(p :: Nil)) => ResponseString(p)
      case req @ PUT(_) => ResponseString(process(Body.string(req)))
      case req @ GET(_) => ResponseString("Post a line to process for: " + this.getClass.getSimpleName)
    }

    unfiltered.jetty.Http(port).filter(plan).run()
  }
}
