import org.joda.time.DateTime

object BotKernel extends PollCommands(DataEntities(Map.empty, Map.empty, Map.empty))
{
  def TimerAutoController(): Unit = {
    dataBase.polls.foreach(m => {
      if(m._2.timeStart != null){
        if((m._2.timeStart.GetTimeMili() < DateTime.now().getMillis) &&
          (m._2.status == PollHasNotYetBeenLaunched)){
          dataBase = dataBase.copy(polls = dataBase.polls + (m._1 -> m._2.copy(status = PollStart)))
        }
      }
      if(m._2.timeStop != null){
        if((m._2.timeStop.GetTimeMili() < DateTime.now().getMillis) &&
          (m._2.status == PollStart)){
          dataBase = dataBase.copy(polls = dataBase.polls + (m._1 -> m._2.copy(status = PollStop)))
        }
      }
    })
  }
}