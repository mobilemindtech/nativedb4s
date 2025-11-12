resolvers += Resolver.sonatypeCentralRepo("snapshots")

import scala.language.postfixOps
import scala.scalanative.build.*
import bindgen.interface.Binding
import bindgen.plugin.BindgenMode
import com.indoorvivants.detective.Platform

ThisBuild / scalaVersion := "3.7.4"
ThisBuild / organization := "io.nativedb4s"
ThisBuild / name := "nativedb4s"

scalacOptions ++= Seq(
  "-new-syntax",
  "-Wvalue-discard",
  "-Wunused:all",
  "-deprecation",
  "-explain",
  "-explain-cyclic",
  "-rewrite",
  "-source:future"
)

lazy val common = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("common"))
  .settings(
    name := "common"
  )

// crossProject para JVM + Native
lazy val api = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("api"))
  .dependsOn(common)
  .settings(
    name := "api"
  )
lazy val apiJVM = api.jvm
lazy val apiNative = api.native

lazy val mysql = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("mysql"))
  .enablePlugins(
    ScalaNativePlugin,
    BindgenPlugin,
    VcpkgNativePlugin,
    ScalaNativeJUnitPlugin
  )
  .dependsOn(api)
  .settings(
    name := "mysql",
    logLevel := Level.Info
  )
  .nativeSettings(
    vcpkgDependencies := VcpkgDependencies("libmysql", "openssl", "zlib"),
    vcpkgNativeConfig ~= { _.addRenamedLibrary("libmysql", "mysqlclient") },
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.6.0"
    ),
    nativeConfig ~= { c =>
      c.withLinkingOptions(c.linkingOptions.flatMap {
        case "-lresolv-lresolv" => Some("-lresolv")
        case "-lm-lresolv"      => None
        case other              => Some(other)
      } ++ Seq("-lmysqlclient", "-lstdc++"))
        .withLTO(LTO.none)
        .withMode(Mode.debug)
        .withGC(GC.immix)
        .withSourceLevelDebuggingConfig(_.enableAll)
        .withOptimize(false)
        .withMode(scalanative.build.Mode.debug)
        .withCompileOptions(c.compileOptions ++ Seq("-g"))
    },
    bindgenBindings += {
      val actualIncludeFolder = new File(
        vcpkgConfigurator.value.pkgConfig
          .compilationFlags("mysqlclient")
          .toList
          .filter(_.contains("include/mysql"))
          .head
          .stripPrefix("-I")
      )

      Binding(actualIncludeFolder / "mysql.h", "libmysql")
        .withLinkName("mysqlclient")
        .withCImports(List("mysql/mysql.h"))
        .withClangFlags(
          vcpkgConfigurator.value.pkgConfig
            .updateCompilationFlags(List("-std=gnu99"), "mysqlclient")
            .toList
        )
    },
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v")
  )
  .nativeSettings(bindgenSettings)
  .nativeSettings(configurePlatform())

val bindgenSettings = Seq(
  bindgenMode := BindgenMode.Manual(
    scalaDir =
      (Compile / sourceDirectory).value / "scala" / "io" / "nativedb4s" / "mysql" / "bindings",
    cDir = (Compile / resourceDirectory).value / "scala-native" / "mysql"
  ),
  bindgenBindings := {
    bindgenBindings.value.map(_.withNoLocation(true))
  }
)

def configurePlatform(rename: String => String = identity) = Seq(
  nativeConfig := {
    val conf = nativeConfig.value
    val arch64 =
      if (
        Platform.arch == Platform.Arch.Arm && Platform.bits == Platform.Bits.x64
      )
        List("-arch", "arm64")
      else Nil

    conf
      .withLinkingOptions(conf.linkingOptions ++ arch64)
      .withCompileOptions(conf.compileOptions ++ arch64)
  }
)
