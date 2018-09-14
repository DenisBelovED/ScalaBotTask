import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative.Commands

import scala.io.Source

object Bot extends TelegramBot with Polling with Commands{
  lazy val token: String = Source.fromFile("src\\main\\scala\\bot.token").getLines().mkString
  onCommand(
    "create_poll",
    "list",
    "delete_poll",
    "start_poll",
    "stop_poll",
    "result",
    "begin",
    "end",
    "view",
    "add_question",
    "delete_question",
    "answer") {
    implicit msg => {
      BotKernel.TimerAutoController()
      val s = BotKernel.Parse(msg.text.get)
      if(s.isFailure)
        reply("Ошибка")
      s.get match {
        case command: CreatePollCommand =>
          reply("Опрос создан, его id: " +
            BotKernel.CreatePoll(
              msg.from.get.id,
              command.name,
              command.anonymous,
              command.visibility,
              command.timeStart,
              command.timeStop,
              reply).toString
          )
        case command: ListCommand =>
          reply(
            BotKernel.List
          )
        case command: DeletePollCommand =>
          reply(
            BotKernel.DeletePoll(command.id, msg.from.get.id).toString + " опрос " + command.id + " удалён"
          )
        case command: StartPollCommand =>
          reply(
            BotKernel.StartPoll(command.id, msg.from.get.id).toString + " опрос " + command.id + " запущен"
          )
        case command: StopPollCommand =>
          reply(
            BotKernel.StopPoll(command.id, msg.from.get.id).toString + " опрос " + command.id + " остановлен"
          )
        case command: ResultCommand =>
          reply(
            BotKernel.Result(command.id, msg.from.get.id, reply)
          )
        case command: BeginCommand =>
          reply(
            BotKernel.Begin(command.id, msg.from.get.id).toString
          )
        case command: EndCommand =>
          reply(
            BotKernel.End(msg.from.get.id).toString
          )
        case command: ViewCommand =>
          reply(
            BotKernel.View(msg.from.get.id, reply)
          )
        case command: AddQuestionCommand =>
          reply(
            BotKernel.AddQuestion(
              msg.from.get.id,
              command.question,
              command.questionType,
              command.answers.toVector,
              reply
            )
          )
        case command: DeleteQuestionCommand =>
          reply(
            BotKernel.DeleteQuestion(command.id, msg.from.get.id, reply).toString
          )
        case command: AnswerCommand =>
          reply(
            BotKernel.Answer(msg.from.get.id, command.id, command.answer, reply).toString
          )
      }
    }
  }
}
