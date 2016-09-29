name := "eff-test"

scalaVersion := "2.11.8"

libraryDependencies += "org.atnos" %% "eff-cats" % "2.0.0-RC9"

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")
