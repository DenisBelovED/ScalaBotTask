import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.scalatest._

import scala.util.Success


class ParserTests extends FlatSpec with Matchers {
  private val parse = (x: String) => CommandParser.parse(x)
  private val timeFrom = (x: String) =>
    LocalDateTime.parse(x, DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))

  "A Parser" should "parse create_poll correctly" in {
    val start = "21:00:59 02:01:30"
    val end = "23:40:59 05:01:29"

    assertResult(parse("/create_poll (name)"))(Success(CreatePollCommand("name")))
    assertResult(parse("/create_poll ((())) (no)"))(Success(CreatePollCommand("( )", false)))
    assertResult(parse("/create_poll (name) ( yes )   (continuous) " + s"($start)"))(Success(CreatePollCommand("name", anonymous = true, Continuous, start)))
    assertResult(parse("/create_poll(name)(yes)(continuous)" + s"($start)($end)"))(Success(CreatePollCommand("name", anonymous = true, Continuous, start, end)))
  }

  it should "parse add_question correctly" in {
    assertResult(parse("/add_question (name) (open)")) (Success(AddQuestionCommand("name", Open)))
    assertResult(parse("/add_question (name) (choice)\n(o)")) (Success(AddQuestionCommand("name", Choice, List("o"))))
    assertResult(parse("/add_question (name) (multi)\n(o)")) (Success(AddQuestionCommand("name", Multi, List("o"))))
  }

  it should "parse answer correctly" in{
    assertResult(parse("/answer (1) ()")) (Success(AnswerCommand(1, "")))
    assertResult(parse("/answer (1) (1 1 1)")) (Success(AnswerCommand(1, "1 1 1")))
  }

  it should "parse create_poll incorrectly" in {
    assert(parse("/create_poll").isFailure)
    assert(parse("/create_poll ((anonymity)").isFailure)
  }

  it should "parse add_question incorrectly" in {
    assert(parse("/add_question ").isFailure)
    assert(parse("/add_question (name").isFailure)
    assert(parse("add_question ").isFailure)
  }

  it should "parse answer_question incorrectly" in {
    assert(parse("/answer_question (1)").isFailure)
    assert(parse("answer_question").isFailure)
  }

  def incorrectParseWithoutArgument(command : String) : Unit = {
    assert(parse(s"/$command (1)").isFailure)
    assert(parse(s"$command ").isFailure)
    assert(parse(s"/$command (").isFailure)
    assert(parse(s"/$command )").isFailure)
    assert(parse(s"/$command ((1)").isFailure)
  }
  def incorrectParseWithoutArgument(commands : Seq[String]) : Unit =
    for(command <- commands) incorrectParseWithoutArgument(command)

  def correctParseWithoutArgument(string : String, command: Command) : Unit =
    assertResult(parse(s"/$string")) (Success(command))
  def correctParseWithoutArgument(commands : Seq[(String, Command)]) : Unit =
    for ((string, command) <- commands) correctParseWithoutArgument(string, command)


  def incorrectParseWithOneArgument(command : String) : Unit = {
    assert(parse(s"/$command ()").isFailure)
    assert(parse(s"$command ()").isFailure)
    assert(parse(s"$command").isFailure)
  }
  def incorrectParseWithOneArgument(commands : Seq[String]) : Unit =
    for (command <- commands ) incorrectParseWithOneArgument(command)

  def correctParseWithOneArgument( string : String, command : Command) : Unit =
    assertResult(parse(s"/$string (1)")) (Success(command))
  def correctParseWithOneArgument( commands : Seq[(String, Command)]) : Unit =
    for ((string, command) <- commands) correctParseWithOneArgument(string, command)

  it should " parse command with one argument correctly" in
    correctParseWithOneArgument(List( ("delete_poll", DeletePollCommand(1)) ,
      ("start_poll", StartPollCommand(1)), ("stop_poll", StopPollCommand(1)), ("result", ResultCommand(1)),
      ("begin", BeginCommand(1)), ("delete_question", DeleteQuestionCommand(1))) )

  it should "parse command with one argument incorrectly" in
    incorrectParseWithOneArgument(List("delete_poll", "start_poll", "stop_poll", "result", "begin", "delete_question"))

  it should "parse command without arguments correctly " in
    correctParseWithoutArgument(List( ("list", ListCommand()), ("view", ViewCommand()), ("end", EndCommand()) ))

  it should "parse command without arguments incorrectly" in
    incorrectParseWithoutArgument(List("list", "end", "view"))
}