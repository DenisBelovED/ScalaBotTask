case class Poll (
            creatorId: Int,
            pollId: Long,
            name: String,
            anonymoys: Boolean = true,
            visibility: PollResultVisibility = AfterStop,
            timeStart: DateTimeSolver = null,
            timeStop: DateTimeSolver = null,
            status: PollStatus = PollHasNotYetBeenLaunched,
            var answers: Map[(Int, Long, Int), String] = Map.empty
          )
