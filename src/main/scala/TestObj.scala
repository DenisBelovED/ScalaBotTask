object TestObj {
  def main(args: Array[String]): Unit = {
    /*val t = new DateTimeSolver()
    t.SetTime("01:01:01 21:12:01")
    val poll: Long = PollCommands().CreatePoll("lol", anonymity = false, timeStart = t)*/
    BotKernel.CreatePoll(777, "azino"/*, timeStartStr = ""*/, timeStopStr = "01:01:01 20:12:01")
    BotKernel.CreatePoll(777, "azino2"/*, timeStartStr = ""*/, timeStartStr = "01:01:01 20:12:01")
    for (polls <- BotKernel.dataBase.polls){
      for(cont <- BotKernel.dataBase.contexts){
        println(cont)
      }
    }
    println("-------------------------")
    for (polls <- BotKernel.dataBase.polls){
      println(polls)
    }
    println("-------------------------")
    BotKernel.List
    println("-------------------------")
    for (polls <- BotKernel.dataBase.polls){
      BotKernel.StartPoll(polls._2.pollId, 777)
    }
    BotKernel.List
    for (polls <- BotKernel.dataBase.polls){
      BotKernel.StopPoll(polls._2.pollId, 777)
    }
    BotKernel.List
    println("-------------------------")
    for (polls <- BotKernel.dataBase.polls){
      BotKernel.DeletePoll(polls._2.pollId, 777)
    }
    println("-------------------------")
    for (polls <- BotKernel.dataBase.polls){
      println(polls)
    }
    println("-------------------------")
    BotKernel.List
  }
}
