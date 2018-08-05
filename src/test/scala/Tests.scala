import org.scalatest._

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

  "DateTimeSolver tests GetTimeMili()" should "TimeNotStated при CorrectTime" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 20:12:01") == CorrectTime should be (true)
    time.GetTimeMili() != TimeNotStated  should be (true)
  }
  it should "TimeNotStated при IncorrectTime" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 17:12:01") == IncorrectTime should be (true)
    time.GetTimeMili() == TimeNotStated  should be (true)
  }
  it should "TimeNotStated при InvalidTimeFormat" in {
    val time = new DateTimeSolver()
    time.SetTime("01:01:01 12:01") == InvalidTimeFormat should be (true)
    time.GetTimeMili() == TimeNotStated  should be (true)
  }
}
