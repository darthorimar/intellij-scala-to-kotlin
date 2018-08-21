package darthorimar.intellijScalaToKotlin

class DebugConveterTest extends ConverterTestBase {
  def testDebug(): Unit =
    doTest(
      """ trait I
        |  case class A(i: I) extends I
        |  case class B(x: Int) extends I
        |  val q =  (B(1) : I) match {
        |    case A(B(1) | B(2)) => 42
        |  }""".stripMargin,"", doPrint = true)

}
