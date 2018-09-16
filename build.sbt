name:= "Merc"

version := "0.2"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.3"

libraryDependencies += "org.mockito" % "mockito-core" % "1.9.5" % "test"

libraryDependencies += "com.projectdarkstar.ext.jorbis" % "jorbis" % "0.0.17"

libraryDependencies += "de.huxhorn.sulky" % "de.huxhorn.sulky.3rdparty.jlayer" % "1.0"

libraryDependencies += "com.miglayout" % "miglayout-javafx" % "4.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

libraryDependencies += "org.imgscalr" % "imgscalr-lib" % "4.2"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-RC1"

libraryDependencies += "com.typesafe" % "config" % "1.3.2"

libraryDependencies += "com.github.andr83" %% "scalaconfig" % "0.4"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:postfixOps", "-language:implicitConversions",
  "-language:reflectiveCalls", "-Ypartial-unification")

jfxSettings

JFX.mainClass := Some("mr.merc.main.Main")

JFX.devKit := JFX.jdk(System.getenv("JAVA_HOME"))

JFX.addJfxrtToClasspath := false

JFX.nativeBundles := "image"

fork := true