name := "fun-regex-matcher"

version := "1.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation")

libraryDependencies += "jparsec" % "jparsec" % "2.0.1"

libraryDependencies += "com.ibm.icu" % "icu4j" % "50.1"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"
