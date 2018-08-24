case class Poll (
            creatorId: Int, // ID user
            pollId: Long, // poll id
            name: String, // poll header
            anonymoys: Boolean = true,
            visibility: PollResultVisibility = AfterStop,
            timeStart: DateTimeSolver = null,
            timeStop: DateTimeSolver = null,
            status: PollStatus = PollHasNotYetBeenLaunched,
            answers: Map[(Int, Long, Int), String] = Map.empty // answers
          )
