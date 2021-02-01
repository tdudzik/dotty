package dotty.dokka

import java.nio.file.Path
import java.nio.file.Paths
import dotty.dokka.model.api._
import dotty.tools.dotc.core.Contexts.Context

enum SocialLinks(val url: String, val whiteIconName: String, val blackIconName: String):
  case Github(ghUrl: String) extends SocialLinks(ghUrl, "github-icon-white.png", "github-icon-black.png")
  case Twitter(tUrl: String) extends SocialLinks(tUrl, "twitter-icon-white.png", "twitter-icon-black.png")
  case Gitter(gUrl: String) extends SocialLinks(gUrl, "gitter-icon-white.png", "gitter-icon-black.png")
  case Discord(dUrl: String) extends SocialLinks(dUrl, "discord-icon-white.png", "discord-icon-black.png")
  case Custom(cUrl: String, cWhiteIconName: String, cBlackIconName: String) extends SocialLinks(cUrl, cWhiteIconName, cBlackIconName)

object SocialLinks:
  def parse(s: String): Either[String, SocialLinks] =
    val splitted = s.split("::")
    if splitted.size < 2 then Left(s"Social links arg $s is invalid.") else
      splitted.head match {
        case "github" => Right(Github(splitted(1)))
        case "twitter" => Right(Twitter(splitted(1)))
        case "gitter" => Right(Gitter(splitted(1)))
        case "discord" => Right(Discord(splitted(1)))
        case "custom" if splitted.size == 4 => Right(Custom(splitted(1), splitted(2), splitted(3)))
        case _ => Left(s"Social links arg $s is invalid.")
      }
