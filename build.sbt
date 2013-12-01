name:= "Merc"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.3"

libraryDependencies += "org.scalafx" % "scalafx_2.10" % "1.0.0-M6"

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test"

libraryDependencies += "org.mockito" % "mockito-core" % "1.9.5"

libraryDependencies += "com.projectdarkstar.ext.jorbis" % "jorbis" % "0.0.17"

libraryDependencies += "de.huxhorn.sulky" % "de.huxhorn.sulky.3rdparty.jlayer" % "1.0"

unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

mainClass := Some("mr.merc.main.Main")

scalacOptions ++= Seq("-feature", "-deprecation", "-language:postfixOps", "-language:implicitConversions")