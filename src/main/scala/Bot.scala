import scala.io.Source
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.methods.ParseMode

object Bot extends TelegramBot with Polling with Commands{
  lazy val token = Source.fromFile("src\\main\\scala\\bot.token").getLines().mkString

  //onCommand('create_poll)
}
