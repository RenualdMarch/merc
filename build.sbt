name:= "Merc"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.3"

libraryDependencies += "org.scalafx" % "scalafx_2.10" % "1.0.0-M6"

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test"

libraryDependencies += "org.mockito" % "mockito-core" % "1.9.5" % "test"

libraryDependencies += "com.projectdarkstar.ext.jorbis" % "jorbis" % "0.0.17"

libraryDependencies += "de.huxhorn.sulky" % "de.huxhorn.sulky.3rdparty.jlayer" % "1.0"

unmanagedJars in Test += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

scalacOptions ++= Seq("-feature", "-deprecation", "-language:postfixOps", "-language:implicitConversions")

jfxSettings

JFX.mainClass := Some("mr.merc.main.Main")

JFX.devKit := JFX.jdk(System.getenv("JAVA_HOME"))

JFX.addJfxrtToClasspath := true

JFX.nativeBundles := "image"
