class ContextManager {
  def Begin(id: Long): ContextStatus =
  {
    //TODO сообщение об успехе или ошибка
    ContextBegin
  }

  def End(id: Long): ContextStatus =
  {
    //TODO сообщение об успехе или ошибка
    ContextEnd
  }

  def View(): String =
  {
    //TODO Красивое представление опроса в !контексте которого мы находимся!
    "Красивое представление опроса"
  }

  def AddQuestion(
                   question: String,
                   typeQuestion: QuestionType,
                   answers: String*
                 ): Long =
  {
    1
  }

  def DeleteQuestion(id: Long): QuestionStatus =
  {
    //TODO сообщение об успехе или ошибка
    QuestionSuccessDelete
  }

  def Answer(id: Long, answer: String): AnswerStatus =
  {
    //TODO сообщение об успехе или ошибка
    AnswerCorrect
  }
}
