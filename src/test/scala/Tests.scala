import org.scalatest._
import BotKernel._

class Tests extends FlatSpec with Matchers{
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

  "BotKernel test TimerAutoController" should "проверка автостарта и автостопа" in {
    val user1 = 1234567890
    val timeStart = "03:13:01 18:08:25"
    val timeStop = "03:19:01 18:08:25"
    val pollId = CreatePoll(user1, "name", true, Continuous, timeStart, timeStop)
    Thread.sleep(300000)
    TimerAutoController()
    TimerAutoController()
    dataBase.polls(pollId).status == PollStart should be (true)
    Thread.sleep(300000)
    TimerAutoController()
    TimerAutoController()
    dataBase.polls(pollId).status == PollStop should be (true)
  }
}
