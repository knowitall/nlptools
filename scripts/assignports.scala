import scala.sys.process._

val assemblies = Seq("find", "-name", "*assembly*jar").lines
var port = 12001
for (assembly <- assemblies) {
  val Array(_, tool, system, _ @ _*) = assembly.split("/")
  println(s"/$tool/$system/\thttp://localhost:$port/\t$assembly")
  port = port + 1
}
