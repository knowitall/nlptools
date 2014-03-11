import sbt._
import sbt.Keys._

import com.typesafe.sbt.SbtNativePackager
import com.typesafe.sbt.SbtNativePackager.Universal

object Package {

  lazy val settings = SbtNativePackager.packagerSettings ++ 
    SbtNativePackager.packageArchetype.java_application ++
    Seq(
      mappings in Universal ++= 
        (sourceDirectory.value / "main" / "bin" ** "*" x relativeTo(sourceDirectory.value / "main")) :+
        (file("scripts") / "nlptools-run-server.sh" -> "bin/nlptools-run-server.sh")
    )
}
