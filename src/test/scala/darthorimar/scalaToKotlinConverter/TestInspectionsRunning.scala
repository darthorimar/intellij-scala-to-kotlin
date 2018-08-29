package darthorimar.scalaToKotlinConverter

class TestInspectionsRunning extends ConverterTestBase {
  def testExtraBracketsInspection(): Unit =
    doExprTest(
      """val a = ((1 + 1))""".stripMargin,
      """val a: Int = 1 + 1""".stripMargin)

  def testPropertyAccessSyntaxInspection(): Unit =
    doTest(
      """def foo(thread: Thread) {
        |    thread.setDaemon(true)
        |}
        |""".stripMargin,
      """fun foo(thread: Thread) {
        |    thread.daemon = true
        |}
        |""".stripMargin)

  def testMoveToLambdaInspection(): Unit = //todo fix
    doExprTest(
      """val a = Seq(1).fold(1)((acc, q) => acc + q )""".stripMargin,
      """val a: Int = listOf(1).fold(1) { acc, q -> acc + q }""".stripMargin)

}
