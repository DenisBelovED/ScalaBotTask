import scala.util.Try

case class PollCommands(var dataBase: DataEntities) {
  def CreatePoll(
                  userId: Int,
                  name: String,
                  anonymity: Boolean = true,
                  visibility: PollResultVisibility = AfterStop,
                  timeStartStr: String = null,
                  timeStopStr: String = null): Any =
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
    }
  }

  def List: String =
  {
    val answer = dataBase.polls.map(
      t => {"user id = " + t._2.creatorId + " : poll id = " + t._2.pollId + " : " + t._2.name + " - " + t._2.status}
    ).mkString("\n")
    CommandsResponseAlarms.ListWriter(answer)
    answer
  }

  def DeletePoll(pollId: Long, userId: Int): PollStatus = {
    if(dataBase.polls(pollId).creatorId == userId){
      if (Try(dataBase = dataBase.copy(polls = dataBase.polls - pollId, contexts = dataBase.contexts - pollId)).isSuccess){
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
        case PollStop =>
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
        case PollStop =>
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
      CommandsResponseAlarms.StartPollPremissionError()
      PollPrivilegeError
    }
  }

  def Result(pollId: Long, userId: Int): String =
  {
    //TODO Красивый отчет о текущем/прошедшем голосовании
    "Красивый отчет о текущем/прошедшем голосовании"
  }
}
