name:= "Merc"

version := "0.2"

// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" %% "scalafx" % "14-R19"

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "14.0.1" classifier osName
)

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.3"

libraryDependencies += "org.mockito" % "mockito-core" % "2.23.0" % "test"

libraryDependencies += "com.miglayout" % "miglayout-javafx" % "5.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

libraryDependencies += "org.imgscalr" % "imgscalr-lib" % "4.2"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

libraryDependencies += "com.typesafe" % "config" % "1.3.2"

libraryDependencies += "org.kordamp.ikonli" % "ikonli-javafx" % "11.3.4"

libraryDependencies += "org.kordamp.ikonli" % "ikonli-fontawesome-pack" % "11.3.4"

libraryDependencies += "org.kordamp.ikonli" % "ikonli-entypo-pack" % "11.3.4"

libraryDependencies += "com.object-refinery" % "orsoncharts" % "1.7"

libraryDependencies += "com.esotericsoftware" % "kryo" % "4.0.2"

libraryDependencies += "com.twitter" %% "chill" % "0.9.1"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:postfixOps",
  "-language:implicitConversions", "-language:reflectiveCalls", "-Ypartial-unification")

fork := true

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}