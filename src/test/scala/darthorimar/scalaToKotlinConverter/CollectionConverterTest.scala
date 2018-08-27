package darthorimar.scalaToKotlinConverter

class CollectionConverterTest extends ConverterTestBase {
  def testOptionConverters(): Unit =
    doExprTest(" Some(1).map(x => x + 1).get",
      "1?.let { x -> x + 1}!!")

  def testOptionCall(): Unit =
    doExprTest("Option(1)", "1")

  def testOptionGetOrElse(): Unit =
    doExprTest(" Some(1).getOrElse(2)",
      "1 :? 2")

  def testListCon(): Unit =
    doExprTest(
      "1 :: Nil",
      "listOf(1) + emptyList()")

  def testSeqMmpty(): Unit =
    doExprTest(
      """Seq.empty[Int]""".stripMargin,
      """emptyList<Int>()""".stripMargin)

  def testMkString(): Unit =
    doExprTest(
      """Seq.empty.mkString("(", ",", ")" )""".stripMargin,
      """emptyList().joinToString(",", "(", ")")""".stripMargin)

  def testSeqTail(): Unit =
    doExprTest(
      """Seq.empty.tail""".stripMargin,
      """emptyList().drop(1)""".stripMargin)

  def testSeqInit(): Unit =
    doExprTest(
      """Seq.empty.init""".stripMargin,
      """emptyList().dropLast(1)""".stripMargin)

  def testSeqHead(): Unit =
    doExprTest(
      """Seq.empty.head""".stripMargin,
      """emptyList().first()""".stripMargin)

  def testSeqApply(): Unit =
    doExprTest(
      """val s = Seq(1,2)
        |s(0)
      """.stripMargin,
      """val s: List<Int> = listOf(1, 2)
        |s[0]""".stripMargin)

  def testSeqConcat(): Unit =
    doExprTest(
      """val s1 = Seq(1,2)
        |val s2 = Seq(2,3)
        |s1 ++ s2
      """.stripMargin,
      """val s1: List<Int> = listOf(1, 2)
        |val s2: List<Int> = listOf(2, 3)
        |s1 + s2""".stripMargin)

  def testSeqNotEmpty(): Unit =
    doExprTest(
      """Seq(1,2).nonEmpty()
      """.stripMargin,
      """listOf(1, 2).isNotEmpty()""".stripMargin)

  def testSeqSize(): Unit =
    doExprTest(
      """Seq(1,2).size()
      """.stripMargin,
      """listOf(1, 2).size""".stripMargin)

  def testSeqOfOptionFlatten(): Unit =
    doExprTest(
      """Seq(Some(1),None).flatten""".stripMargin,
      """listOf(1, null).filterNotNull()""".stripMargin)


  def testStringRepeat(): Unit =
    doExprTest(
      """ "nya" * 4""".stripMargin,
      """ "nya".repeat(4)""".stripMargin)

  def testPairConstruct(): Unit =
    doExprTest(
      """ 1 -> 2 """.stripMargin,
      """ 1 to 2""".stripMargin)

  def testPairComponents(): Unit =
    doExprTest(
      """ (1 -> 2)._1 """.stripMargin,
      """ (1 to 2).first""".stripMargin)

  def testPairType(): Unit =
    doTest(
      """ def foo(a: (Int, String)) =  a
        | def bar(a: (Int, String, Char)) =  a
      """.stripMargin,
      """
        |fun foo(a: Pair<Int, String>): Pair<Int, String> =a
        | fun bar(a: Tuple3<Int, String, Char>): Tuple3<Int, String, Char> =a
      """.stripMargin)

  def testTryApply(): Unit =
    doTest(
      """ import scala.util.Try
        |val a: Try[Int] = Try(1)
      """.stripMargin,
      """val a: Try<Int> = runTry { 1 }
         |""".stripMargin)





}