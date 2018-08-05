object TestObj {
  def main(args: Array[String]): Unit = {
    val t = new DateTimeSolver()
    t.SetTime("01:01:01 21:12:01")
    val poll: Unit = PollCommands().CreatePoll("lol", anonymity = false, timeStart = t)
  }
}
