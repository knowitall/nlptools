package edu.washington.cs.knowitall.tool

import edu.washington.cs.knowitall.common.main.{ LineProcessor => CliLineProcessor }
import unfiltered.request._
import unfiltered.response._
import unfiltered.filter.Planify

abstract class LineProcessor extends CliLineProcessor {
  override def main(args: Array[String]) = {
    if (args.exists(_ == "--server")) {
      runServer(args filterNot (_ == "--server"))
    }
    else {
      super.main(args)
    }
  }

  def runServer(args: Array[String]) = {
    val port = args.dropWhile(_ != "--port").drop(1).headOption match {
      case None => 8080
      case Some(p) => p.toInt
    }

    val plan = Planify {
      case Path(Seg(p :: Nil)) => ResponseString(p)
      case req @ PUT(_) => ResponseString(process(Body.string(req)))
      case req @ GET(_) => ResponseString("Post a line to process for: " + this.getClass.getSimpleName)
    }

    unfiltered.jetty.Http(port).filter(plan).run()
  }
}
