name:= "Merc"

version := "0.2"

scalaVersion := "2.10.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.3"

libraryDependencies += "org.scalafx" % "scalafx_2.10" % "8.0.0-R4"

libraryDependencies += "org.mockito" % "mockito-core" % "1.9.5" % "test"

libraryDependencies += "com.projectdarkstar.ext.jorbis" % "jorbis" % "0.0.17"

libraryDependencies += "de.huxhorn.sulky" % "de.huxhorn.sulky.3rdparty.jlayer" % "1.0"

libraryDependencies += "com.miglayout" % "miglayout-javafx" % "4.2"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:postfixOps", "-language:implicitConversions", "-language:reflectiveCalls")

jfxSettings

JFX.mainClass := Some("mr.merc.main.Main")

JFX.devKit := JFX.jdk(System.getenv("JAVA_HOME"))

JFX.addJfxrtToClasspath := false

JFX.nativeBundles := "image"
