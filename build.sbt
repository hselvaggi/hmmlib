scalaVersion := "2.11.8"
fork := true

libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-java8-compat" % "0.7.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
scalacOptions ++= Seq(
  "-Xexperimental", //enable SAM closures
  "-deprecation",
  "-unchecked", //warn about certain unchecked constrains by the compiler
  "-feature", //warn against usage of restricted features when non declared with import language.<feature>
  "-Yinfer-argument-types", //useful for extending types
  "-Ybackend:GenBCode",
  "-Ydelambdafy:method",
  "-target:jvm-1.8",
  "-Xlint" //lint! because it's good.
)
