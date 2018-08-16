sealed trait PollResultVisibility
case object AfterStop extends PollResultVisibility
case object Continuous extends PollResultVisibility

sealed trait TimeStatus
case object CorrectTime extends TimeStatus
case object IncorrectTime extends TimeStatus
case object TimeNotStated extends TimeStatus
case object InvalidTimeFormat extends TimeStatus

sealed trait PollStatus
case object PollSuccessDelete extends PollStatus
case object PollFailedDelete extends PollStatus
case object PollPrivilegeError extends PollStatus
case object PollNotExist extends PollStatus
case object PollStart extends PollStatus
case object PollStop extends PollStatus

sealed trait ContextStatus
case object ContextBegin extends ContextStatus
case object ContextNotExist extends ContextStatus
case object ContextEnd extends ContextStatus

sealed trait QuestionType
case object Open extends QuestionType
case object Choice extends QuestionType
case object Multi extends QuestionType

sealed trait QuestionStatus
case object QuestionSuccessDelete extends QuestionStatus
case object QuestionNotExist extends QuestionStatus

sealed trait AnswerStatus
case object AnswerCorrect extends AnswerStatus
case object AnswerIncorrect extends AnswerStatus