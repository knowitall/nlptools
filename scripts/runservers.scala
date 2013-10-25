import scala.sys.process._

val portFile = args(0)

println("Server tools specified in: " + portFile)

for (line <- io.Source.fromFile(portFile).getLines) {
  val Array(path, port, jar) = line.split("\t")

  print(s"Serving $path at $port ($jar): ")
  val ec = Seq("java", "-jar", jar, "--server", "--port", port).!
  println(ec)
}
