sealed trait PollResultVisibility
case object AfterStop extends PollResultVisibility
case object Continuous extends PollResultVisibility

sealed trait TimeStatus
case object CorrectTime extends TimeStatus
case object IncorrectTime extends TimeStatus
case object TimeNotStated extends TimeStatus
case object InvalidTimeFormat extends TimeStatus
