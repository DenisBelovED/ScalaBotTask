object TestObj {
  def main(args: Array[String]): Unit = {
    /*val t = new DateTimeSolver()
    t.SetTime("01:01:01 21:12:01")
    val poll: Long = PollCommands().CreatePoll("lol", anonymity = false, timeStart = t)*/
    BotKernel.CreatePoll(777, "azino"/*, timeStartStr = ""*/, timeStopStr = "01:01:01 20:12:01")
    BotKernel.CreatePoll(777, "azino2"/*, timeStartStr = ""*/, timeStartStr = "01:01:01 20:12:01")
    for (polls <- BotKernel.dataBase.polls){
        println(polls._2.context)
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
    /*println("-------------------------")
    for (polls <- BotKernel.dataBase.polls){
      BotKernel.DeletePoll(polls._2.pollId, 777)
    }
    println("-------------------------")*/
    println("-------------------------")
    val id = BotKernel.CreatePoll(777, "azino3", visibility = AfterStop, anonymity = false)
    //BotKernel.dataBase = BotKernel.dataBase.copy(answers = BotKernel.dataBase.answers + (id -> (1 -> "lol")))
    BotKernel.Result(id)
    BotKernel.StartPoll(id, 777)
    BotKernel.Result(id)
    BotKernel.dataBase.polls(id).context =
    BotKernel.dataBase.polls(id).context :+
      Map(1 -> "a") :+ Map(2 -> "a") :+ Map(3 -> "a") :+
      Map(1 -> "b") :+ Map(2 -> "b") :+ Map(3 -> "c") :+
      Map(4 -> "lol")

    BotKernel.StopPoll(id, 776)
    BotKernel.StopPoll(id, 777)
    BotKernel.Result(id)
    println("-------------------------")
    for (polls <- BotKernel.dataBase.polls){
      println(polls)
    }
    println("-------------------------")
    BotKernel.List
  }
}
