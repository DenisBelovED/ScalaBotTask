case class PollCommands() {
  def CreatePoll
  (
    name: String,
    anonymity: Boolean = true,
    visibility: PollResultVisibility = AfterStop,
    timeStart: DateTimeSolver = null,
    timeStop: DateTimeSolver = null
  ): Unit =
  {
    println(name, anonymity, visibility, timeStart, timeStop)
  }
}
