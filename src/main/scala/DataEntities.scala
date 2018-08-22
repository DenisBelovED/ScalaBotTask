case class DataEntities(
                         polls: Map[Long, Poll], // pollId, poll
                         contexts: Map[Int, Long], // userId, pollId
                         questions: Map[Long, Question] // question uuid, question
                       )