name:= "Merc"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.3"

libraryDependencies += "org.scalafx" % "scalafx_2.10" % "1.0.0-M4"

//libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"

//libraryDependencies += "joda-time" % "joda-time" % "2.1"

//libraryDependencies += "org.joda" % "joda-convert" % "1.1"

unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

mainClass := Some("mr.merc.main.Main")