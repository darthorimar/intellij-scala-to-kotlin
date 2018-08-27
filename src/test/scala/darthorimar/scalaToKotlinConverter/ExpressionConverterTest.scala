package darthorimar.scalaToKotlinConverter

class ExpressionConverterTest extends ConverterTestBase {

  def testBoolExpression(): Unit =
    doExprTest(
      """!(!true || false) && false""".stripMargin,
      """!(!true || false) && false""".stripMargin)

  def testPrefixExpr(): Unit =
    doExprTest(
      """!true""".stripMargin,
      """!true""".stripMargin)

  def testIntExpr(): Unit =
    doExprTest(
      """(1) + (4 / 2 - 3)""".stripMargin,
      """(1) + (4 / 2 - 3)""".stripMargin)

  def testStringConcat(): Unit =
    doExprTest(
      """ "1" + "2" """.stripMargin,
      """ "1" + "2" """.stripMargin)

  def testDoubles(): Unit =
    doExprTest(
      """ val a = 1.0 + 2 """.stripMargin,
      """ val a: Double = 1.0 + 2""".stripMargin)

  def testFloats(): Unit =
    doExprTest(
      """ val a = 1.0f + 2 """.stripMargin,
      """ val a: Float = 1.0f + 2""".stripMargin)



  def testForComprehension(): Unit =
    doExprTest(
      """for {
        |  i <- Seq(1,2)
        |  j <- Seq(2,3)
        |  a = i
        |  if a > 2
        |} {
        |   println(i + j)
        |}""".stripMargin,
      """
        |for (i: Int in listOf(1, 2)) {
        |    for (j: Int in listOf(2, 3)) {
        |      val a = i
        |      if (a > 2){
        |        println(i + j)
        |      }
        |    }
        |  }
      """.stripMargin)



  def testCasts(): Unit =
    doExprTest(
      """1.asInstanceOf[Long]
        |1.isInstanceOf[Long]
      """.stripMargin,
      """(1 as Long)
        |(1 is Long)
      """.stripMargin)

  def testTryFinally(): Unit =
    doExprTest(
      """try { 1 } catch {
        |   case e: Exception => 2
        |   case _ => 3
        |} finally { 5 }""".stripMargin,
      """ try {
        |    1
        |  }catch (e: Exception) {
        |    2
        |  } catch (e: Throwable) {
        |    when {
        |      else -> {
        |        3
        |      }}
        |
        |  } finally {
        |    5
        |  }""".stripMargin)

  def testLambda(): Unit =
    doExprTest(
      """Seq(1).map(x => x + 1)""".stripMargin,
      """listOf(1).map{x -> x + 1}""".stripMargin)

  def testLambdaWithUnderscore(): Unit =
    doExprTest(
      """Seq(1).map(_+1)""".stripMargin,
      """listOf(1).map { it + 1}""".stripMargin)



  def testStringInterpolation(): Unit =
    doExprTest(
      """ s"${1} + $None" """.stripMargin,
      """ "${1} + $null"""".stripMargin)

}
