import scala.util.Try

case class PollCommands(var dataBase: DataEntities) {
  def CreatePoll(
                  userId: Int,
                  name: String,
                  anonymity: Boolean = true,
                  visibility: PollResultVisibility = AfterStop,
                  timeStartStr: String = null,
                  timeStopStr: String = null): Long =
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
        CommandsResponseAlarms.TimeException()
        -1
    }
  }

  def List: String =
  {
    val answer = dataBase.polls.map(
      t => {"user id = " + t._2.creatorId + " : poll id = " + t._2.pollId + " : " + t._2.name + " - " + t._2.status}
    ).mkString("\n")
    CommandsResponseAlarms.DataWriter(answer)
    answer
  }

  def DeletePoll(pollId: Long, userId: Int): PollStatus =
  {
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

  def StartPoll(pollId: Long, userId: Int): PollStatus =
  {
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

  def StopPoll(pollId: Long, userId: Int): PollStatus =
  {
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

  def Result(pollId: Long): String =
  {
    def CountResult: String = {
      val results = dataBase.polls(pollId).context
      if (dataBase.polls(pollId).anonymoys) {
        "всего ответов: " + results.size + "\n" +
          results.flatten
            .map(t => t._2)
            .groupBy(identity)
            .mapValues(_.size)
            .map(t => "За " + t._1 + " проголосовало " + t._2)
            .mkString("\n")
      } else {
        "всего ответов: " + results.size + "\n" +
          results.flatten
            .map(t => t._2)
            .groupBy(identity)
            .mapValues(_.size)
            .map(
              t => "За " + t._1 + " проголосовало " + t._2 + " их id: " +
                results.flatten.groupBy(m => m._2).mapValues(m => m.map(p => p._1))(t._1).mkString(" ")
            )
            .mkString("\n")
      }
    }

    dataBase.polls(pollId).status match {
      case PollHasNotYetBeenLaunched =>
        CommandsResponseAlarms.PollHasNotYetBeenLaunched()
        "PollHasNotYetBeenLaunched"
      case PollStart =>
        dataBase.polls(pollId).visibility match {
          case Continuous =>
            val res = CountResult
            CommandsResponseAlarms.DataWriter("Опрос " + pollId.toString + " активен")
            CommandsResponseAlarms.DataWriter(res)
            res
          case AfterStop =>
            CommandsResponseAlarms.PollNotAble()
            "PollNotAble"
        }
      case PollStop =>
        val res = CountResult
        CommandsResponseAlarms.DataWriter("Опрос " + pollId.toString + " остановлен")
        CommandsResponseAlarms.DataWriter(res)
        res
      case _ =>
        CommandsResponseAlarms.PollException()
        "PollException"
    }
  }
}
