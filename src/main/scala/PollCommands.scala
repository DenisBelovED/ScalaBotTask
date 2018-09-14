import info.mukel.telegrambot4s.methods.ParseMode.ParseMode
import info.mukel.telegrambot4s.models.{Message, ReplyMarkup}

import scala.concurrent.Future
import scala.util.Try

case class PollCommands(var dataBase: DataEntities) {
  def CreatePoll(
                  userId: Int,
                  name: String,
                  anonymity: Boolean = true,
                  visibility: PollResultVisibility = AfterStop,
                  timeStartStr: String = null,
                  timeStopStr: String = null,
                  reply: (String,
                    Option[ParseMode],
                    Option[Boolean],
                    Option[Boolean],
                    Option[Int],
                    Option[ReplyMarkup]) => Future[Message]): Long =
  {
    val pollId = UUIDGenerator.GetUUID()
    val timeStart = new DateTimeSolver()
    val timeStop = new DateTimeSolver()

    val timeStartStatus = timeStart.SetTime(timeStartStr)
    val timeStopStatus = timeStop.SetTime(timeStopStr)

    (timeStartStatus, timeStopStatus) match {
      case (CorrectTime, CorrectTime) =>
        CommandsResponseAlarms.TimeIsCorrect(pollId)
        val poll = Poll(userId, pollId, name, anonymity, visibility, timeStart, timeStop)
        dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> poll))
        pollId
      case (CorrectTime, _) if timeStopStr == null =>
        CommandsResponseAlarms.TimeIsCorrectStart(pollId)
        val poll = Poll(userId, pollId, name, anonymity, visibility, timeStart = timeStart)
        dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> poll))
        pollId
      case (_, CorrectTime) if timeStartStr == null =>
        CommandsResponseAlarms.TimeIsCorrectStop(pollId)
        val poll = Poll(userId, pollId, name, anonymity, visibility, timeStop = timeStop)
        dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> poll))
        pollId
      case (a, b) if (timeStartStr == null) && (timeStopStr == null) =>
        CommandsResponseAlarms.TimeNotSatatement(pollId)
        val poll = Poll(userId, pollId,name, anonymity, visibility)
        dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> poll))
        pollId
      case _ =>
        reply("Неверно указано время", None, None, None, None, None)
        -1
    }
  }

  def List: String =
  {
    val answer = dataBase.polls.map(
      t => {"creator id = " + t._2.creatorId + " : poll id = " + t._2.pollId + " : " + t._2.name + " - " + t._2.status}
    ).mkString("\n")
    answer
  }

  def DeletePoll(pollId: Long, userId: Int): PollStatus =
  {
    if(dataBase.polls.contains(pollId)){
      if(dataBase.polls(pollId).creatorId == userId){
        if (Try(dataBase = dataBase.copy(polls = dataBase.polls - pollId)).isSuccess){
          CommandsResponseAlarms.PollSuccessDelete()
          PollSuccessDelete
        }
        else {
          CommandsResponseAlarms.PollFailedDelete()
          PollFailedDelete
        }
      }
      else {
        CommandsResponseAlarms.PollDeleteError()
        PollPrivilegeError
      }
    }
    else {
      CommandsResponseAlarms.PollNotExist()
      PollNotExist
    }
  }

  def StartPoll(pollId: Long, userId: Int): PollStatus =
  {
    if(dataBase.polls.contains(pollId)){
      if(dataBase.polls(pollId).creatorId == userId){
        dataBase.polls(pollId).status match {
          case PollStart =>
            CommandsResponseAlarms.PollStarted()
            PollStart
          case PollStop | PollHasNotYetBeenLaunched =>
            if (dataBase.polls(pollId).timeStart == null) {
              CommandsResponseAlarms.PollStartFromHandle()
              val poll = dataBase.polls(pollId)
              dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> poll.copy(status = PollStart)))
              PollStart
            }
            else {
              CommandsResponseAlarms.PollStartFromTimer()
              PollStop
            }
          case _ =>
            CommandsResponseAlarms.PollNotExist()
            PollNotExist
        }
      }
      else {
        CommandsResponseAlarms.StartPollPremissionError()
        PollPrivilegeError
      }
    }
    else {
      CommandsResponseAlarms.PollNotExist()
      PollNotExist
    }
  }

  def StopPoll(pollId: Long, userId: Int): PollStatus =
  {
    if(dataBase.polls.contains(pollId)){
      if(dataBase.polls(pollId).creatorId == userId){
        dataBase.polls(pollId).status match {
          case PollStop | PollHasNotYetBeenLaunched =>
            CommandsResponseAlarms.PollStoped()
            PollStop
          case PollStart =>
            if (dataBase.polls(pollId).timeStop == null) {
              CommandsResponseAlarms.PollStopFromHandle()
              val poll = dataBase.polls(pollId)
              dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> poll.copy(status = PollStop)))
              PollStop
            }
            else {
              CommandsResponseAlarms.PollStopFromTimer()
              PollStart
            }
          case _ =>
            CommandsResponseAlarms.PollNotExist()
            PollNotExist
        }
      }
      else {
        CommandsResponseAlarms.StopPollPremissionError()
        PollPrivilegeError
      }
    }
    else {
      CommandsResponseAlarms.PollNotExist()
      PollNotExist
    }
  }

  def Result(pollId: Long,
             userId: Int,
             reply: (String,
               Option[ParseMode],
               Option[Boolean],
               Option[Boolean],
               Option[Int],
               Option[ReplyMarkup]) => Future[Message]): String =
  {
    def CountResult(id: Long, num: Int): String = {
      val results = dataBase.polls(pollId)
        .answers
        .filter(p => (p._1._2 == id) && (p._1._3 == num) && (p._1._1 == userId))
      if (dataBase.polls(pollId).anonymoys) {
        "всего ответов: " + results.size + "\n" +
          results
            .values
            .groupBy(identity)
            .mapValues(_.size)
            .map(t => "За ответ " + t._1 + " проголосовало " + t._2)
            .mkString("\n")
      } else {
        "всего ответов: " + results.size + "\n" +
          results
            .values
            .groupBy(identity)
            .mapValues(_.size)
            .map(
              t => "За ответ " + t._1 + " проголосовало " + t._2 + " их id: " +
                results
                  .groupBy(m => m._2)
                  .mapValues(m => m.map(p => p._1._1))(t._1)
                  .mkString(" ")
            ).mkString("\n")
      }
    }

    if(dataBase.polls.contains(pollId)){
      dataBase.polls(pollId).status match {
        case PollHasNotYetBeenLaunched =>
          CommandsResponseAlarms.PollHasNotYetBeenLaunched()
          "PollHasNotYetBeenLaunched"
        case PollStart =>
          dataBase.polls(pollId).visibility match {
            case Continuous =>
              dataBase.polls(pollId).answers.foreach(p =>
                reply("Вопрос: " + dataBase.questions(p._1._2).qestionData._2 + "\n" +
                CountResult(p._1._2, p._1._3), None, None, None, None, None)
              )
              reply("Опрос " + pollId.toString + " " + dataBase.polls(pollId).name + " активен",
                None, None, None, None, None)
              "ResultWrited"
            case AfterStop =>
              CommandsResponseAlarms.PollNotAble()
              "PollNotAble"
          }
        case PollStop =>
          dataBase.polls(pollId).answers.foreach(p =>
            reply("Вопрос: " + dataBase.questions(p._1._2).qestionData._2 + "\n" +
              CountResult(p._1._2, p._1._3), None, None, None, None, None)
          )
          reply("Опрос " + pollId.toString + " " + dataBase.polls(pollId).name + " остановлен",
            None, None, None, None, None)
          "ResultWrited"
        case _ =>
          CommandsResponseAlarms.PollException()
          "PollException"
      }
    }
    else {
      CommandsResponseAlarms.PollNotExist()
      "PollNotExist"
    }
  }

  def Begin(pollId: Long, userId: Int): ContextStatus =
  {
    if (dataBase.polls.contains(pollId)) {
      dataBase = dataBase.copy(contexts = dataBase.contexts + (userId -> pollId))
      CommandsResponseAlarms.ContextBegin(pollId)
      ContextBegin
    } else {
      CommandsResponseAlarms.ContextNotExist(pollId)
      ContextNotExist
    }
  }

  def End(userId: Int): ContextStatus =
  {
    if(dataBase.contexts.contains(userId)){
      dataBase = dataBase.copy(contexts = dataBase.contexts - userId)
      CommandsResponseAlarms.ContextEnd()
      ContextEnd
    }
    else {
      CommandsResponseAlarms.ContextNotBegin()
      ContextEndError
    }
  }

  def View(userId: Int,
           reply: (String,
             Option[ParseMode],
             Option[Boolean],
             Option[Boolean],
             Option[Int],
             Option[ReplyMarkup]) => Future[Message]): String =
  {
    if(dataBase.contexts.contains(userId)) {
      val pollId = dataBase.contexts(userId)
      dataBase.questions.foreach(m => {
        if(m._2.qestionData._5 == pollId) {
          val res = "\n" +
            "Индекс вопроса : " + m._2.qestionData._1.toString + "\n" +
            "Заголовок вопроса : " + m._2.qestionData._2 + "\n" +
            "Тип вопроса : " + m._2.qestionData._3.toString + "\n" +
            "Варианты ответов\n - " + m._2.qestionData._4.mkString("\n - ")
          reply(res, None, None, None, None, None)
        }
      })
      "Correct view"
    }
    else {
      CommandsResponseAlarms.ContextNotBegin()
      "Out of context error"
    }
  }

  def AddQuestion(
                   userId: Int,
                   question: String,
                   typeQuestion: QuestionType = Open,
                   varAnswers: Vector[String],
                   reply: (String,
                     Option[ParseMode],
                     Option[Boolean],
                     Option[Boolean],
                     Option[Int],
                     Option[ReplyMarkup]) => Future[Message]
                 ): String =
  {
    if(dataBase.contexts.contains(userId)) {
      if(userId == dataBase.polls(dataBase.contexts(userId)).creatorId){
        if(dataBase.polls(dataBase.contexts(userId)).status != PollStart){
          val uuid = UUIDGenerator.GetUUID()
          dataBase = dataBase.copy(
            questions = dataBase.questions + (
              uuid -> Question((dataBase.questions.keys.size + 1, question, typeQuestion, varAnswers, dataBase.contexts(userId)))
              )
          )
          val res = dataBase.questions(uuid)
          reply(
            "Добавлен вопрос: " + res.qestionData._2 + "\n" +
              "Статус: " + res.qestionData._3.toString + "\n" +
              "Варианты ответа:\n - " + res.qestionData._4.mkString("\n - ") + "\n" +
              "Номер: " + res.qestionData._1.toString,
            None, None, None, None, None
          )
          res.qestionData._1.toString
        }
        else {
          reply("Ошибка. Опрос запущен, контекст нельзя изменить",
            None, None, None, None, None)
          "Question context immutable"
        }
      }
      else {
        CommandsResponseAlarms.ContextPremissionError()
        "Context premission error"
      }
    }
    else {
      CommandsResponseAlarms.ContextNotBegin()
      "Out of context error"
    }
  }

  def DeleteQuestion(questionId: Int,
                     userId: Int,
                     reply: (String,
                       Option[ParseMode],
                       Option[Boolean],
                       Option[Boolean],
                       Option[Int],
                       Option[ReplyMarkup]) => Future[Message]): QuestionStatus =
  {
    if(dataBase.contexts.contains(userId)) {
      if(userId == dataBase.polls(dataBase.contexts(userId)).creatorId){
        if(dataBase.polls(dataBase.contexts(userId)).status != PollStart){
          if((0 < questionId) && (questionId <= dataBase.questions.size)) {
            val numToIdMap = dataBase.questions.mapValues(q => q.qestionData._1).map(m => m.swap)
            dataBase = dataBase.copy(questions = dataBase.questions - numToIdMap(questionId))
            val numsIter = (1 to dataBase.questions.size).iterator
            val indexes = dataBase.questions.map(m => m._1 -> numsIter.next())
            dataBase.questions.keys.foreach(key =>
            {
              val t = dataBase.questions(key).qestionData
              dataBase = dataBase.copy(
                questions = dataBase.questions +
                  (key -> dataBase.questions(key).copy(qestionData = (indexes(key), t._2, t._3, t._4, t._5)))
              )
            }
            )
            reply("Вопрос " + questionId.toString + " удалён",
              None, None, None, None, None)
            reply("теперь опрос выглядит так:",
              None, None, None, None, None)
            dataBase.questions.foreach(q =>
              reply(
                q._2.qestionData._1.toString + ") " + q._2.qestionData._2 + "\n - " +
                  q._2.qestionData._4.mkString("\n - "),
                None, None, None, None, None
              )
            )
            QuestionSuccessDelete
          }
          else {
            reply("Ошибка. Указан неверный номер вопроса",
              None, None, None, None, None)
            QuestionFailedDelete
          }
        }
        else {
          reply("Ошибка. Опрос запущен, контекст нельзя изменить",
            None, None, None, None, None)
          QuestionContextImmutable
        }
      }
      else {
        CommandsResponseAlarms.ContextPremissionError()
        QuestionContextPremissionError
      }
    }
    else {
      CommandsResponseAlarms.ContextNotBegin()
      QuestionContextError
    }
  }

  def Answer(userId: Int,
             answerId: Int,
             answer: String,
             reply: (String,
               Option[ParseMode],
               Option[Boolean],
               Option[Boolean],
               Option[Int],
               Option[ReplyMarkup]) => Future[Message]): AnswerStatus =
  {
    def ChoiceVoter(ans: String, uuidQuestion: Long, pollId: Long): AnswerStatus = {
      Try(ans.toInt).map(i => {
        if ((0 < i) && (i <= dataBase.questions(uuidQuestion).qestionData._4.size)){
          val newPollCopy = dataBase.polls(pollId).copy(
            answers = dataBase.polls(pollId).answers + ((userId, uuidQuestion, answerId) -> i.toString)
          )
          dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> newPollCopy))
          reply("Ваш голос принят",
            None, None, None, None, None)
          AnswerCorrect
        }
        else {
          reply("Ошибка. Указанный индекс вопроса не существует",
            None, None, None, None, None)
          AnswerIndexError
        }
      }).getOrElse({
        reply("Ошибка. Вы указали не число",
          None, None, None, None, None)
        AnswerIncorrect
      })
    }

    def MultiVoter(ans: String, uuidQuestion: Long, pollId: Long): AnswerStatus = {
      val splitedAns = ans
        .split(' ')
        .distinct
        .sorted
      val preparedAns = splitedAns.mkString(" ")
      if (!splitedAns.map(n => {
        Try(n.toInt).map(i => {
          if ((0 < i) && (i <= dataBase.questions(uuidQuestion).qestionData._4.size)) {
            val newPollCopy = dataBase.polls(pollId).copy(
              answers = dataBase.polls(pollId).answers + ((userId, uuidQuestion, answerId) -> preparedAns)
            )
            dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> newPollCopy))
            AnswerCorrect
          }
          else {
            AnswerIncorrect
          }
        }).getOrElse({
          AnswerIncorrect
        })
      }).contains(AnswerIncorrect)) {
        reply("Ваш голос принят",
          None, None, None, None, None)
        AnswerCorrect
      } else {
        reply("Ошибка. Вы указали не число или индекс, которого не существует",
          None, None, None, None, None)
        AnswerIndexError
      }
    }

    if(dataBase.contexts.contains(userId)) {
      val pollId = dataBase.contexts(userId)
      if(dataBase.polls(pollId).status == PollStart){
        val mapAnsIdToQuuid = dataBase.questions.filter(m =>
          (m._2.qestionData._5 == pollId) && (m._2.qestionData._1 == answerId)
        ).map(m => m.swap).map(m => m._1.qestionData._1 -> m._2)
        if (mapAnsIdToQuuid.nonEmpty) {
          val uuidQuestion: Long  = mapAnsIdToQuuid(answerId)
          dataBase.questions(uuidQuestion).qestionData._3 match {
            case Open =>
              val newPollCopy = dataBase.polls(pollId).copy(
                answers = dataBase.polls(pollId).answers + ((userId, uuidQuestion, answerId) -> answer)
              )
              dataBase = dataBase.copy(polls = dataBase.polls + (pollId -> newPollCopy))
              reply("Ваш голос принят",
                None, None, None, None, None)
              AnswerCorrect
            case Choice =>
              ChoiceVoter(answer, uuidQuestion, pollId)
            case Multi =>
              MultiVoter(answer, uuidQuestion, pollId)
          }
        }
        else {
          reply(
            "Ошибка. Вы не находитесь в контексте данного опроса",
            None, None, None, None, None
          )
          AnswerIndexError
        }
      }
      else {
        CommandsResponseAlarms.AnswerUnavaliablePollNotStarted()
        AnswerUnavaliablePollNotRun
      }
    }
    else {
      CommandsResponseAlarms.ContextNotBegin()
      AnswerContextError
    }
  }
}