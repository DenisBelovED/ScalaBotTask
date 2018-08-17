case class Poll (
            creatorId: Int,
            pollId: Long,
            name: String,
            anonymoys: Boolean = true,
            visibility: PollResultVisibility = AfterStop,
            timeStart: DateTimeSolver = null,
            timeStop: DateTimeSolver = null,
            status: PollStatus = PollHasNotYetBeenLaunched,
            var context: Vector[Map[Int, String]] = Vector.empty
          ) extends ContextManager
{

}
