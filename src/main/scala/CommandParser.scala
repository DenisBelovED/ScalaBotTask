import scala.util.Try
import scala.util.parsing.combinator._

object CommandParser extends RegexParsers {

  val string: Parser[String] = "[^()]+".r
  val digit: Parser[Int] = "\\d+".r ^^ (_.toInt)
  val date: Parser[String] = "(\\d{2}:){2}\\d{2} (\\d{2}:){2}\\d{2}".r

  val stringArg: Parser[String] =
    "(" ~> rep("((" ^^^ "(" | "))" ^^^ ")" | string) <~ ")" ^^ (s => s.mkString(sep = " "))
  val digitArg: Parser[Int] = "(" ~> digit <~ ")"
  val dateArg: Parser[String] = "(" ~> date <~ ")"


  val isAnonymous: Parser[Boolean] =  "(" ~> ("yes" | "no") <~ ")" ^^ { _ == "yes" }
  val isVisible: Parser[PollResultVisibility] = "(" ~> ("afterstop" | "continuous") <~ ")" ^^ {
    case "afterstop" => AfterStop
    case "continuous" => Continuous
  }
  val qType: Parser[QuestionType] = "(" ~> ("multi" | "open" | "choice") <~ ")" ^^ {
    case "multi" => Multi
    case "open" => Open
    case "choice" => Choice
  }

  def createPoll: Parser[CreatePollCommand] =
    "/create_poll" ~> stringArg ~ opt(isAnonymous ~ opt(isVisible ~ opt(dateArg ~ opt(dateArg)))) ^^ {
      case name ~ None =>                                               CreatePollCommand(name)
      case name ~ Some(anon ~ None) =>                                  CreatePollCommand(name, anon)
      case name ~ Some(anon ~ Some(vis ~ None)) =>                      CreatePollCommand(name, anon, vis)
      case name ~ Some(anon ~ Some(vis ~ Some(sTime ~ None))) =>        CreatePollCommand(name, anon, vis, sTime)
      case name ~ Some(anon ~ Some(vis ~ Some(sTime ~ Some(eTime)))) => CreatePollCommand(name, anon, vis, sTime, eTime)
    }
  def list: Parser[ListCommand] = "/list".r ^^^ ListCommand()
  def deletePoll: Parser[DeletePollCommand] = "/delete_poll" ~> digitArg ^^ { dg => DeletePollCommand(dg) }
  def startPoll: Parser[StartPollCommand] = "/start_poll" ~> digitArg ^^ { dg => StartPollCommand(dg) }
  def stopPoll: Parser[StopPollCommand] = "/stop_poll" ~> digitArg ^^ { dg => StopPollCommand(dg) }
  def result: Parser[ResultCommand] = "/result" ~> digitArg ^^ { dg => ResultCommand(dg) }
  def begin: Parser[BeginCommand] = "/begin" ~> digitArg ^^ { dg => BeginCommand(dg) }
  def end: Parser[EndCommand] = "/end".r ^^^ EndCommand()
  def view: Parser[ViewCommand] = "/view".r ^^^ ViewCommand()
  def deleteQuestion: Parser[DeleteQuestionCommand] = "/delete_question" ~> digitArg ^^ { dg => DeleteQuestionCommand(dg) }
  def answer: Parser[AnswerCommand] = "/answer".r ~> digitArg ~ stringArg ^^ {
    case id ~ answer => AnswerCommand(id, answer)
  }
  val addQuestion: Parser[AddQuestionCommand] = "/add_question".r ~>
    stringArg  ~ qType ~rep(stringArg) ^? {
    case name ~ Open ~ options if options.isEmpty => AddQuestionCommand(name, Open)
    case name ~ Multi ~ options if options.nonEmpty => AddQuestionCommand(name, Multi, options)
    case name ~ Choice ~ options if options.nonEmpty => AddQuestionCommand(name, Choice, options)
  }

  def command: Parser[Command] = {
    createPoll | list | deletePoll | startPoll | stopPoll | result |
      begin | end | view | addQuestion | deleteQuestion | answer
  }

  def parse(string: String): Try[Command] = {
    Try {
      CommandParser.parseAll(command, string) match {
        case Success(r, _) => r
        case Failure(_, _) => throw new IllegalArgumentException(s"Bad command!\n$string")
        case Error(_, _) => throw new IllegalArgumentException(s"Bad command!\n$string")
      }
    }
  }
}