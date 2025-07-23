
resolvers += Resolver.sonatypeRepo("snapshots")

import scala.language.postfixOps
import scala.scalanative.build.*
import scala.sys.process.*
import bindgen.interface.Binding
import bindgen.plugin.BindgenMode
import com.indoorvivants.detective.Platform

scalaVersion := "3.7.1"
name := "mysql4s"
organization := "com.mysql4s"

// set to Debug for compilation details (Info is default)
logLevel := Level.Info

lazy val appStop = inputKey[Unit]("stop app")
lazy val appRestart = inputKey[Unit]("run app")
lazy val showPid = inputKey[Unit]("show app PID")

scalacOptions ++= Seq(
  "-new-syntax",
  //"-no-indent",
  "-Wvalue-discard",
  "-Wunused:all",
  //"-Werror",
  "-deprecation",
  "-explain",
  "-explain-cyclic",
  "-rewrite",
  "-source:future",

)

lazy val root = project.in(file(".")).
  enablePlugins(ScalaNativePlugin, BindgenPlugin, VcpkgNativePlugin, ScalaNativeJUnitPlugin).
  settings(

    vcpkgDependencies := VcpkgDependencies("libmysql", "openssl", "zlib"),
    vcpkgNativeConfig ~= { _.addRenamedLibrary("libmysql", "mysqlclient") },

    libraryDependencies ++= Seq(
      //"org.scalameta" %% "munit" % "1.0.0" % Test
      "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.6.0"
    ),

    // defaults set with common options shown
    nativeConfig ~= { c =>

      c.withLinkingOptions(c.linkingOptions.flatMap {
        case "-lresolv-lresolv" => Some("-lresolv")
        case "-lm-lresolv"      => None
        case other              => Some(other)
      }).withLTO(LTO.none) // thin
        .withMode(Mode.debug) // releaseFast
        .withGC(GC.immix)
        //.withLinkingOptions(
        //  c.linkingOptions ++ Seq(
        //    "-lmysqlclient"
        //  )
        //)
        .withSourceLevelDebuggingConfig(_.enableAll) // enable generation of debug informations
        .withOptimize(false)  // disable Scala Native optimizer
        .withMode(scalanative.build.Mode.debug) // compile using LLVM without optimizations
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
    appStop := {
      val logger: TaskStreams = streams.value
      val shell: Seq[String] = if (sys.props("os.name").contains("Windows")) Seq("cmd", "/c") else Seq("bash", "-c")
      val cmdGetPid = Seq(
        "ps", "-ef", "|", "grep", name.value, "|", "grep", "-v", "grep", "|", "awk", "'{print $2}'"
      ).mkString(" ")

      //logger.log.info(s"execute: ${cmdGetPid.mkString(" ")}")
      val pid = ((shell :+ cmdGetPid) !!)
      if (pid.nonEmpty) {

        logger.log.info(s"PID=$pid")

        val cmd = Seq(
          "kill", "-s", "9", pid
        ).mkString(" ")

        val result = ((shell :+ cmd) ! logger.log)
        if(result == 0){
          logger.log.success(s"stop app successful")
        } else {
          logger.log.success("stop app failure")
        }
      } else {
        logger.log.info("app is not running")
      }
    },

    showPid := {
      val logger: TaskStreams = streams.value
      val shell: Seq[String] = if (sys.props("os.name").contains("Windows")) Seq("cmd", "/c") else Seq("bash", "-c")
      val cmd = Seq(
        "ps", "-ef", "|", "grep", name.value, "|", "grep", "-v", "grep", "|", "awk", "'{print $2}'"
      ).mkString(" ")

      //logger.log.info(s"execute: ${cmd.mkString(" ")}")
      val pid = (shell :+ cmd) !!

      if(pid.nonEmpty){
        logger.log.info(s"PID=$pid")
      }else{
        logger.log.info(s"pid not found")
      }
    },
    appRestart := {
      val logger: TaskStreams = streams.value
      logger.log.info("app restart..")
      appStop.evaluated
      (Compile / run).evaluated
    }
  )
  .settings(bindgenSettings)
  .settings(configurePlatform())

val bindgenSettings = Seq(
  bindgenMode := BindgenMode.Manual(
    scalaDir = (Compile / sourceDirectory).value / "scala" / "io" / "mysql4s" / "bindings",
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
      .withLinkingOptions(
        conf.linkingOptions ++ arch64
      )
      .withCompileOptions(
        conf.compileOptions ++ arch64
      )
  }
)

addCommandAlias("run", "appStart")

//ThisBuild/usePipelining := true

testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v")
