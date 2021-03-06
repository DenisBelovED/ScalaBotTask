sealed trait PollResultVisibility
case object AfterStop extends PollResultVisibility
case object Continuous extends PollResultVisibility

sealed trait TimeStatus
case object CorrectTime extends TimeStatus
case object IncorrectTime extends TimeStatus
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
case object AnswerContextError extends AnswerStatus
case object AnswerIndexError extends AnswerStatus
case object AnswerUnavaliablePollNotRun extends AnswerStatus

sealed trait Command
case class CreatePollCommand (name: String, // poll header
                              anonymous: Boolean = true,
                              visibility: PollResultVisibility = AfterStop,
                              timeStart: String = null,
                              timeStop: String = null
                             ) extends Command
case class ListCommand () extends Command
case class DeletePollCommand (id: Long) extends Command
case class StartPollCommand (id: Long) extends Command
case class StopPollCommand (id: Long) extends Command
case class ResultCommand (id: Long) extends Command
case class BeginCommand (id: Long) extends Command
case class EndCommand () extends Command
case class ViewCommand () extends Command
case class AddQuestionCommand (question: String,
                               questionType: QuestionType = Choice,
                               answers: List[String] = Nil
                              ) extends Command
case class DeleteQuestionCommand (id: Int) extends Command
case class AnswerCommand (id: Int, answer: String) extends Command