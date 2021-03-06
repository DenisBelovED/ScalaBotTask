import BotKernel._
import org.scalatest._

import scala.util.Try

class Tests extends FlatSpec with Matchers {
  "DateTimeSolver tests SetTime()" should "CorrectTime правильное время" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 20:12:01") == CorrectTime should be (true)
  }
  it should "CorrectTime -> CorrectTime изменение правильного времени на правильное" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 20:12:01") == CorrectTime should be (true)
    time.SetTime("01:01:01 21:12:01") == CorrectTime should be (true)
  }
  it should "IncorrectTime -> CorrectTime изменение неправильного времени на правильное" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 17:12:01") == IncorrectTime should be (true)
    time.SetTime("01:01:01 21:12:01") == CorrectTime should be (true)
  }
  it should "CorrectTime -> IncorrectTime изменение правильного времени на неправильное" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 21:12:01") == CorrectTime should be (true)
    time.SetTime("01:01:01 17:12:01") == IncorrectTime should be (true)
  }
  it should "InvalidTimeFormat -> CorrectTime изменение времени неверного формата на правильный формат" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 12:01") == InvalidTimeFormat should be (true)
    time.SetTime("01:01:01 21:12:01") == CorrectTime should be (true)
  }
  it should "IncorrectTime прошедшее время" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 17:12:01") == IncorrectTime should be (true)
  }
  it should "InvalidTimeFormat лишние символы" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:0a 17:12:01") == InvalidTimeFormat should be (true)
  }
  it should "InvalidTimeFormat выход за границы времени" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:70 17:12:01") == InvalidTimeFormat should be (true)
  }
  it should "InvalidTimeFormat недостаточный ввод" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01 17:12:01") == InvalidTimeFormat should be (true)
  }
  it should "InvalidTimeFormat избыточный ввод" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 2017:12:01") == IncorrectTime should be (true)
  }

  "DateTimeSolver tests GetTimeMili()" should "-1 при CorrectTime" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 20:12:01") == CorrectTime should be (true)
    time.GetTimeMili() != -1  should be (true)
  }
  it should "-1 при IncorrectTime" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 17:12:01") == IncorrectTime should be (true)
    time.GetTimeMili() == -1  should be (true)
  }
  it should "-1 при InvalidTimeFormat" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 12:01") == InvalidTimeFormat should be (true)
    time.GetTimeMili() == -1  should be (true)
  }

  /*"BotKernel test TimerAutoController" should "проверка автостарта и автостопа" in {
    val user1 = 1234567890
    val timeStart = "03:13:01 18:08:25"
    val timeStop = "03:19:01 18:08:25"
    val pollId = CreatePoll(user1, "name", true, Continuous, timeStart, timeStop, reply = null)
    Thread.sleep(300000)
    TimerAutoController()
    TimerAutoController()
    dataBase.polls(pollId).status == PollStart should be (true)
    Thread.sleep(300000)
    TimerAutoController()
    TimerAutoController()
    dataBase.polls(pollId).status == PollStop should be (true)
  }*/

  "BotKernel test CreatePoll" should "проверка получения poll id в зависимости от входных параметров времени" in {
    val user1 = 1234567890
    val user2 = 987654321
    val user3 = 1111111111
    val user4 = 22222222

    val timeStart = "03:13:01 20:08:25"
    val timeStop = "03:19:01 21:08:25"
    val pollId = CreatePoll(user1, "name", true, Continuous, timeStart, timeStop, reply = null)
    pollId > 0 should be (true)

    val timeStart2 = null
    val timeStop2 = "03:19:01 21:08:25"
    val pollId2 = CreatePoll(user1, "name", true, Continuous, timeStart2, timeStop2, reply = null)
    pollId2 > 0 should be (true)

    val timeStart3 = "03:19:01 21:08:25"
    val timeStop3 = null
    val pollId3 = CreatePoll(user2, "name", true, Continuous, timeStart3, timeStop3, reply = null)
    pollId3 > 0 should be (true)

    val timeStart4 = null
    val timeStop4 = null
    val pollId4 = CreatePoll(user3, "name", true, Continuous, timeStart4, timeStop4, reply = null)
    pollId4 > 0 should be (true)
    val timeStart5 = "abracadabra"
    val timeStop5 = null
    Try {
      val pollId5 = CreatePoll(user4, "name", true, Continuous, timeStart5, timeStop5, reply = null)
      pollId5 == -1 should be (true)
    }.isFailure should be (true)
  }

  "BotKernel test List" should "проверка команды List" in {
    val user1 = 1234567890
    val pollId = CreatePoll(user1, "name", false, reply = null)
    StartPoll(pollId, user1)
    List != "" should be (true)
    StopPoll(pollId, user1)
    List != "" should be (true)
  }

  "BotKernel test DeletePoll" should "проверка команды DeletePoll" in {
    val user1 = 984593612
    val anotherUser = 1234123
    val pollId1 = CreatePoll(user1, "forDelete", false, reply = null)
    DeletePoll(pollId1, anotherUser) == PollPrivilegeError should be (true)
    DeletePoll(pollId1, user1) == PollSuccessDelete should be (true)
    DeletePoll(pollId1, anotherUser) == PollNotExist should be (true)
    DeletePoll(pollId1, user1) == PollNotExist should be (true)
  }

  "BotKernel test StartPoll" should "проверка команды StartPoll" in {
    val user1 = 984593612
    val anotherUser = 1234123
    val pollId1 = CreatePoll(user1, "forStart", false, reply = null)
    StartPoll(pollId1, anotherUser) == PollPrivilegeError should be (true)
    StartPoll(pollId1, user1) == PollStart should be (true)
    StartPoll(pollId1+1, anotherUser) == PollNotExist should be (true)
    StartPoll(pollId1+1, user1) == PollNotExist should be (true)
  }

  "BotKernel test StopPoll" should "проверка команды StopPoll" in {
    val user1 = 984593612
    val anotherUser = 1234123
    val pollId1 = CreatePoll(user1, "forStop", false, reply = null)
    StartPoll(pollId1, anotherUser) == PollPrivilegeError should be (true)
    StartPoll(pollId1, user1) == PollStart should be (true)
    StartPoll(pollId1+1, anotherUser) == PollNotExist should be (true)
    StartPoll(pollId1+1, user1) == PollNotExist should be (true)

    StopPoll(pollId1, anotherUser) == PollPrivilegeError should be (true)
    StopPoll(pollId1, user1) == PollStop should be (true)
    StopPoll(pollId1+1, anotherUser) == PollNotExist should be (true)
    StopPoll(pollId1+1, user1) == PollNotExist should be (true)
  }

  "BotKernel test Result" should "проверка команды Result" in {
    val user1 = 984593612
    val pollId1 = CreatePoll(user1, "Result", false, Continuous, reply = null)
    Result(pollId1+1, user1, reply = null) == "PollNotExist" should be (true)
    Result(pollId1, user1, reply = null) == "PollHasNotYetBeenLaunched" should be (true)
    StartPoll(pollId1, user1) == PollStart should be (true)
    Try {
      Result(pollId1, user1, reply = null) != "ResultWrited" should be (true)
    }.isFailure should be (true)
    StopPoll(pollId1, user1) == PollStop should be (true)
    Try {
      Result(pollId1, user1, reply = null) != "ResultWrited" should be (true)
    }.isFailure should be (true)

    val pollId2 = CreatePoll(user1, "Result2", false, AfterStop, reply = null)
    Result(pollId2+1, user1, reply = null) == "PollNotExist" should be (true)
    Result(pollId2, user1, reply = null) == "PollHasNotYetBeenLaunched" should be (true)
    StartPoll(pollId2, user1) == PollStart should be (true)
    Result(pollId2, user1, reply = null) == "PollNotAble" should be (true)
    StopPoll(pollId2, user1) == PollStop should be (true)
    Try {
      Result(pollId2, user1, reply = null) != "ResultWrited" should be (true)
    }.isFailure should be (true)
  }

  "BotKernel test context" should "проверка команд Begin, View, End" in {
    val user1 = 984593612
    val pollId1 = CreatePoll(user1, "Context", false, Continuous, reply = null)
    View(user1, reply = null) == "Out of context error" should be (true)
    Begin(pollId1+1, user1) == ContextNotExist should be (true)
    Begin(pollId1, user1) == ContextBegin should be (true)
    View(user1, reply = null) != "" should be (true)
    End(user1+1) == ContextEndError should be (true)
    End(user1) == ContextEnd should be (true)
    View(user1, reply = null) == "Out of context error" should be (true)
  }
  it should "проверка команды AddQuestion" in {
    val user1 = 984593612
    val user2 = 111111111
    val pollId1 = CreatePoll(user1, "ContextAddQuestion", false, Continuous, reply = null)
    AddQuestion(user1, "q1", Open, Vector("s", "d", "f", "g"), reply = null) == "Out of context error" should be (true)
    Begin(pollId1, user1)
    Begin(pollId1, user2)
    AddQuestion(user2, "q1", Open, Vector("s", "d", "f", "g"), reply = null) == "Context premission error" should be (true)
    StartPoll(pollId1, user1)
    Try{
      AddQuestion(user1, "q1", Open, Vector("s", "d", "f", "g"), reply = null) == "Question context immutable" should be (true)
    }.isFailure should be (true)
    StopPoll(pollId1, user1)
    Try{
      AddQuestion(user1, "q1", Open, Vector("s", "d", "f", "g"), reply = null) == "1" should be (true)
      DeleteQuestion(1, user1, reply = null)
    }.isFailure should be (true)
  }
  it should "проверка команды DeleteQuestion" in {
    val user1 = 984593612
    val user2 = 111111111
    val pollId1 = CreatePoll(user1, "ContextDeleteQuestion", false, Continuous, reply = null)
    Begin(pollId1, user1)
    Begin(pollId1, user2)
    Try{
      AddQuestion(user1, "q1", Open, Vector("s", "d", "f", "g"), reply = null)
    }.isFailure should be (true)

    DeleteQuestion(1, user2, reply = null) == QuestionContextPremissionError should be (true)
    StartPoll(pollId1, user1)
    Try{
      DeleteQuestion(1, user1, reply = null) == QuestionContextImmutable should be (true)
      DeleteQuestion(2, user1, reply = null) == QuestionFailedDelete should be (true)
      DeleteQuestion(1, user1, reply = null) == QuestionSuccessDelete should be (true)
      DeleteQuestion(1, user1, reply = null) == QuestionFailedDelete should be (true)
    }.isFailure should be (true)
    StopPoll(pollId1, user1)
  }
  it should "проверка команды Answer" in {
    val user3 = 945438634
    val user4 = 376537832
    val pollId1 = CreatePoll(user3, "ContextDeleteQuestion", false, Continuous, reply = null)
    Begin(pollId1, user3)
    Try{
      AddQuestion(user3, "Open", Open, Vector("s", "d", "f", "g"), reply = null)
    }.isFailure should be (true)
    Answer(user4, 1, "open", reply = null) == AnswerContextError should be (true)
    Answer(user3, 1, "open", reply = null) == AnswerUnavaliablePollNotRun should be (true)
    StartPoll(pollId1, user3)
    Try{
      Answer(user3, 2, "open", reply = null) == AnswerIndexError should be (true)
      Answer(user3, 1, "open", reply = null) == AnswerCorrect should be (true)
    }.isFailure should be (true)
    Answer(user4, 2, "open", reply = null) == AnswerContextError should be (true)
    Answer(user4, 1, "open", reply = null) == AnswerContextError should be (true)
    Begin(pollId1, user4)
    Try{
      Answer(user4, 2, "open", reply = null) == AnswerIndexError should be (true)
      Answer(user4, 1, "open", reply = null) == AnswerCorrect should be (true)
    }.isFailure should be (true)
    StopPoll(pollId1, user3)
    Answer(user3, 1, "open", reply = null) == AnswerUnavaliablePollNotRun should be (true)
    Answer(user4, 2, "open", reply = null) == AnswerUnavaliablePollNotRun should be (true)
    Try{
      DeleteQuestion(1, user3, reply = null)
      AddQuestion(user3, "Choice", Choice, Vector("s", "d", "f", "g"), reply = null)
      AddQuestion(user3, "Multi", Multi, Vector("s", "d", "f", "g"), reply = null)
      StartPoll(pollId1, user3)
      Answer(user3, 1, "1", reply = null) == AnswerCorrect should be (true)
      Answer(user4, 1, "1", reply = null) == AnswerCorrect should be (true)
      Answer(user3, 1, "s", reply = null) == AnswerIncorrect should be (true)
      Answer(user4, 1, "6", reply = null) == AnswerIndexError should be (true)
      Answer(user4, 1, "1 2", reply = null) == AnswerIncorrect should be (true)
      Answer(user3, 2, "1", reply = null) == AnswerCorrect should be (true)
      Answer(user4, 2, "1", reply = null) == AnswerCorrect should be (true)
      Answer(user3, 2, "s", reply = null) == AnswerIndexError should be (true)
      Answer(user4, 2, "6", reply = null) == AnswerIndexError should be (true)
      Answer(user4, 2, "3 4", reply = null) == AnswerCorrect should be (true)
      Answer(user4, 2, "3 5", reply = null) == AnswerIndexError should be (true)
    }.isFailure should be (true)
  }
}
