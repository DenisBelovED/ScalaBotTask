import org.joda.time._
import org.joda.time.format.DateTimeFormat

class DateTimeSolver{
  private var dateTime: DateTime = _

  def SetTime(timeStr: String): Any ={
    try{
      dateTime = DateTimeFormat.forPattern("hh:mm:ss yy:MM:dd").parseDateTime(timeStr)
      if (DateTime.now().getMillis > dateTime.getMillis)
        {
          dateTime = null
          IncorrectTime
        } else CorrectTime
    }
    catch {
      case e: Exception =>
        dateTime = null
        InvalidTimeFormat
    }
  }

  def GetTimeMili() : Any = {
    if (dateTime == null) TimeNotStated else dateTime.getMillis
  }
}

/*
класс позволяет проверить указанное время на корректность, и получить указаное время в виде числа
пример:
    val time = new DateTimeSolver() - создали объект
    time.SetTime("01:01:01 12:01:05") - указали время:
        если оно правильное, то возвращает TimeStatus - CorrectTime
        иначе другие TimeStatus, все они описаны в файле StatusTypes
    time.GetTimeMili() - получаем Long, или TimeNotStated, в зависимости от того, какой был TimeStatus при указании времени
 */