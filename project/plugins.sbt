
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("releases")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.8")
addSbtPlugin("com.indoorvivants" % "bindgen-sbt-plugin" % "0.2.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.5")
addSbtPlugin("com.indoorvivants.vcpkg" % "sbt-vcpkg-native" % "0.0.21")