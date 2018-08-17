import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative.Commands

import scala.io.Source

object Bot extends TelegramBot with Polling with Commands{
  lazy val token = Source.fromFile("src\\main\\scala\\bot.token").getLines().mkString
  //onCommand('list) { implicit msg => reply(BotKernel.List(msg.from.get.id)) }
  onCommand("/l") { implicit msg =>
    withArgs { args =>
      reply(
        "hi"
      )
    }
  }
}
