package dotty.tools.scaladoc

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import dotty.tools.scaladoc.test.BuildInfo
import java.nio.file.Path;
import org.jsoup.Jsoup
import util.IO

class JavadocExternalLocationProviderIntegrationTest
    extends ExternalLocationProviderIntegrationTest(
      "externalJavadoc",
      List(".*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/"),
      List(
        "https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.Builder.html",
        "https://docs.oracle.com/javase/8/docs/api/java/util/Map.Entry.html",
        "https://docs.oracle.com/javase/8/docs/api/java/util/Map.html",
        "https://docs.oracle.com/javase/8/docs/api/java/util/ArrayList.html#forEach-java.util.function.Consumer-",
        "https://docs.oracle.com/javase/8/docs/api/java/util/ArrayList.html#toArray-T:A-",
        "https://docs.oracle.com/javase/8/docs/api/java/util/ArrayList.html#subList-int-int-",
        "https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html#printf-java.lang.String-java.lang.Object...-",
        "https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html#write-byte:A-int-int-"
      )
    )

class Scaladoc2ExternalLocationProviderIntegrationTest
    extends ExternalLocationProviderIntegrationTest(
      "externalScaladoc2",
      List(".*scala.*::scaladoc2::https://www.scala-lang.org/api/current/"),
      List(
        "https://www.scala-lang.org/api/current/scala/util/matching/Regex$$Match.html",
        "https://www.scala-lang.org/api/current/scala/Predef$.html#String",
        "https://www.scala-lang.org/api/current/scala/collection/immutable/Map.html",
        "https://www.scala-lang.org/api/current/scala/collection/IterableOnceOps.html#addString(b:StringBuilder,start:String,sep:String,end:String):StringBuilder",
        "https://www.scala-lang.org/api/current/scala/collection/IterableOnceOps.html#mkString(start:String,sep:String,end:String):String"
      )
    )

class Scaladoc3ExternalLocationProviderIntegrationTest
    extends ExternalLocationProviderIntegrationTest(
      "externalScaladoc3",
      List(".*scala.*::scaladoc3::https://dotty.epfl.ch/api/"),
      List(
        "https://dotty.epfl.ch/api/scala/collection/immutable/Map.html",
        "https://dotty.epfl.ch/api/scala/Predef$.html#String-0",
        "https://dotty.epfl.ch/api/scala/util/matching/Regex$$Match.html"
      )
    )

abstract class ExternalLocationProviderIntegrationTest(
    name: String,
    mappings: Seq[String],
    expectedLinks: Seq[String]
) extends ScaladocTest(name):

  override def args = super.args.copy(
    externalMappings = mappings
      .flatMap(s =>
        ExternalDocLink.parse(s).fold(left => None, right => Some(right))
      )
      .toList
  )

  override def runTest = afterRendering {
    val output = summon[DocContext].args.output.toPath.resolve("api")
    val linksBuilder = List.newBuilder[String]

    def processFile(path: Path): Unit =
      val document = Jsoup.parse(IO.read(path))
      val content = document.select(".documentableElement").forEach { elem =>
        val hrefValues = elem.select("a").asScala.map { a =>
          a.attr("href")
        }
        linksBuilder ++= hrefValues
      }

    println(output)
    IO.foreachFileIn(output, processFile)
    val links = linksBuilder.result
    val errors = expectedLinks.flatMap(expect =>
      Option.when(!links.contains(expect))(expect)
    )
    if !errors.isEmpty then {
      val reportMessage =
        "External location provider integration test failed.\n" +
          "Missing links:\n"
          + errors.mkString("\n", "\n", "\n")
          + "Found links:" + links.mkString("\n", "\n", "\n")
      reportError(reportMessage)
    }
  } :: Nil
