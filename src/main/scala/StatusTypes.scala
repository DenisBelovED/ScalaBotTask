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
case object PollHasNotYetBeenLaunched extends PollStatus

sealed trait ContextStatus
case object ContextBegin extends ContextStatus
case object ContextNotExist extends ContextStatus
case object ContextEnd extends ContextStatus
case object ContextEndError extends ContextStatus

sealed trait QuestionType
case object Open extends QuestionType
case object Choice extends QuestionType
case object Multi extends QuestionType

sealed trait QuestionStatus
case object QuestionSuccessDelete extends QuestionStatus
case object QuestionFailedDelete extends QuestionStatus
case object QuestionNotExist extends QuestionStatus
case object QuestionContextError extends QuestionStatus
case object QuestionContextPremissionError extends QuestionStatus
case object QuestionContextImmutable extends QuestionStatus

sealed trait AnswerStatus
case object AnswerCorrect extends AnswerStatus
case object AnswerIncorrect extends AnswerStatus