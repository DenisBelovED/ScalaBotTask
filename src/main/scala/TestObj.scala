object TestObj {
  def main(args: Array[String]): Unit = {
    /*val t = new DateTimeSolver()
    t.SetTime("01:01:01 21:12:01")
    val poll: Long = PollCommands().CreatePoll("lol", anonymity = false, timeStart = t)*/
    BotKernel.CreatePoll(777, "azino"/*, timeStartStr = ""*/, timeStopStr = "01:01:01 20:12:01")
    BotKernel.CreatePoll(777, "azino2"/*, timeStartStr = ""*/, timeStartStr = "01:01:01 20:12:01")
    for (polls <- BotKernel.dataBase.polls){
        println(polls._2.answers)
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
    val id2 = BotKernel.CreatePoll(797, "azino228", visibility = AfterStop, anonymity = false)
    //BotKernel.dataBase = BotKernel.dataBase.copy(answers = BotKernel.dataBase.answers + (id -> (1 -> "lol")))
    /*BotKernel.Result(id)
    BotKernel.StartPoll(id, 777)
    BotKernel.Result(id)
    BotKernel.dataBase.polls(id).answers =
    BotKernel.dataBase.polls(id).answers :+
      Map(1 -> "a") :+ Map(2 -> "a") :+ Map(3 -> "a") :+
      Map(1 -> "b") :+ Map(2 -> "b") :+ Map(3 -> "c") :+
      Map(4 -> "lol")*/

    println("-------------------------------")
    BotKernel.View(777)
    BotKernel.View(797)
    println("-------------------------------")

    BotKernel.StartPoll(id, 777)
    BotKernel.Begin(id2, 777)
    BotKernel.Begin(id2, 797)

    BotKernel.AddQuestion(797, "ВИШНЁВАЯ СЕМЁРКА?", varAnswers = Vector("глухая тонировка", "неоновые фары"))
    BotKernel.AddQuestion(797, "СЕРАЯ ДЕСЯТКА?", varAnswers = Vector("Жора", "Рома"))
    BotKernel.AddQuestion(797, "ЧЁРНАЯ ПРИОРА?", varAnswers = Vector("едут", "они", "куда-то"))



    println("-------------------------------")
    BotKernel.View(777)
    BotKernel.View(797)
    println("-------------------------------")

    BotKernel.AddQuestion(777, "как поднять бабла? куда нажать?", varAnswers = Vector("ааазино", "три", "топора"))
    BotKernel.StopPoll(id, 777)

    println("-------------------------------")
    BotKernel.View(777)
    BotKernel.View(797)
    println("-------------------------------")

    BotKernel.AddQuestion(777, "как поднять бабла? куда нажать?2", varAnswers = Vector("ааазино2", "три2", "топора2"))
    BotKernel.AddQuestion(777, "как поднять бабла? куда нажать?3", varAnswers = Vector("ааазино3", "три3", "топора3"))

    println("-------------------------------")
    BotKernel.View(777)
    BotKernel.View(797)
    println("-------------------------------")

    BotKernel.StartPoll(id, 777)
    BotKernel.DeleteQuestion(2, 777)
    BotKernel.DeleteQuestion(1, 797)

    println("-------------------------------")
    BotKernel.View(777)
    BotKernel.View(797)
    println("-------------------------------")

    BotKernel.StopPoll(id, 777)
    BotKernel.DeleteQuestion(2, 777)
    BotKernel.DeleteQuestion(1, 777)

    println("-------------------------------")
    BotKernel.View(777)
    BotKernel.View(797)
    println("-------------------------------")

    BotKernel.End(777)
    BotKernel.End(797)

    println("-------------------------------")
    BotKernel.View(777)
    BotKernel.View(797)
    println("-------------------------------")


    /*BotKernel.StopPoll(id, 776)
    BotKernel.StopPoll(id, 777)
    BotKernel.Begin(id, 228)
    BotKernel.Begin(id, 227)
    BotKernel.End(227)
    BotKernel.End(228)
    BotKernel.End(228)*/
    BotKernel.Result(id)
    println("-------------------------")
    for (polls <- BotKernel.dataBase.polls){
      println(polls)
    }
    println("-------------------------")
    BotKernel.List
  }
}
