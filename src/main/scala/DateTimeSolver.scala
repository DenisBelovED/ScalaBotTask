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
