import org.scalatest._

class Tests extends FlatSpec with Matchers{
  val exceptionCode1 = 228

  "TestObj.test1" should "вывод на экран 228" in {
    TestObj.test1() should be (exceptionCode1)
  }
}
