case class Poll (
            creatorId: Int,
            pollId: Long,
            name: String,
            anonymoys: Boolean = true,
            visibility: PollResultVisibility = AfterStop,
            timeStart: DateTimeSolver = null,
            timeStop: DateTimeSolver = null,
            status: PollStatus = PollHasNotYetBeenLaunched,
            var answers: Vector[Map[Int, (Long, String)]] = Vector.empty
          )
