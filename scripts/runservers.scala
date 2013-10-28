import scala.sys.process._

val portFile = args(0)

println("Server tools specified in: " + portFile)

var processes = List.empty[(Process, String)]
for (line <- io.Source.fromFile(portFile).getLines) {
  val Array(path, port, jar) = line.split("\t")

  print(s"Serving $path at $port ($jar): ")
  val pc = Seq("java", "-jar", jar, "--server", "--port", port).run
  processes ::= (pc, path)
}

println("Waiting for processes to exit: ")
for ((pc, path) <- processes) {
  val ec = pc.exitValue()
  println(ec + ":\t" + path)
}
