package darthorimar.scalaToKotlinConverter

import scala.util.Try

class ConvertTest extends ConverterTestBase {

  def testFuncCall(): Unit = {
    doExprTest(
      """ "ny".substring(1,2)""",
      """  "ny".substring(1,2)""")
  }

  def testVararg(): Unit =
    doTest(
      """def foo(xs: String*) = xs""".stripMargin,
      """fun foo(vararg xs: String): List<String> =xs""".stripMargin)

  def testUncarry(): Unit = {
    doTest(
      """def a(x: Int, b: String)(c: Char) = 1
        |def b = a(1,"2")('3')
      """.stripMargin,
      """fun a(x: Int, b: String, c: Char): Int =1
        |fun b(): Int =a(1, "2", '3')""".stripMargin)
  }

  def testOverride(): Unit =
    doTest(
      """
        | class A {
        |      def a: Int = 5
        |    }
        |    class B extends A {
        |      def a: Int = 42
        |    }
      """.stripMargin,
      """
        |open class A() {
        |  fun a(): Int =5
        |}
        |open class B() : A() {
        |  override fun a(): Int =42
        |}""".stripMargin)

  def testImplicitLambda(): Unit =
    doExprTest(
      """
        |Seq(1,2,3).map {
        |    case x if x >= 3 => x - 3
        |  case _ => 0
        | }
      """.stripMargin,
      """listOf(1, 2, 3).map { val match = it
        |when {
        |   match is Int && match >= 3 -> {
        |    match - 3
        |  }
        |  else -> {
        |    0
        |  }}
        | }""".stripMargin)


  def testSimpleValDef(): Unit =
    doTest(
      """val a = 5""".stripMargin,
      """val a: Int = 5""".stripMargin)

  def testValInClass(): Unit =
    doTest(
      """class A {
        |  val a: Int
        |  val b = 32
        |  var c: Int
        |  var d = 1
        |}
      """.stripMargin,
      """open class A() {
        |   val a: Int
        |   val b: Int = 32
        |   var c: Int
        |   var d: Int = 1
        |}
      """.stripMargin)


  def testClassTypeParams(): Unit =
    doTest(
      """class A[T]""".stripMargin,
      """open class A<T>()""".stripMargin)

  def testImplicits(): Unit =
    doExprTest(
      """ implicit def toStr(a: Int) = a.toString
        |  def q(s: String) = s
        |  println(q(1))""".stripMargin,
      """fun toStr(a: Int): String =a.toString()
        |  fun q(s: String): String =s
        |  println(q(toStr(1)))""".stripMargin)

  def testFunctionTypeParams(): Unit =
    doTest(
      """def a[T] = Seq.empty[T]""".stripMargin,
      """fun<T> a(): List<T> =emptyList<T>()""".stripMargin)

  def testCallByName(): Unit =
    doTest(
      """def a(x: => Int) = x
        |def q = a(1)""".stripMargin,
      """fun a(x: () -> Int): Int =x()
        |fun q(): Int =a { 1 }""".stripMargin)

  def testForYeild(): Unit =
    doTest(
      """val a = for {
        |  i <- Seq(1,2)
        |} yield i
        |""".stripMargin,
      """import kotlin.coroutines.experimental.buildSequence
        |
        |val a: List<Int> = buildSequence {
        |  for (i: Int in listOf(1, 2)) {
        |      yield(i)
        |   }
        |}
      """.stripMargin)


  def testTupleCreate(): Unit =
    doTest(
      """def foo = (1,2,3)
        |def bar = (1,2)
      """.stripMargin,
      """import convertedFromScala.lib.*
        |
        |fun foo(): Tuple3<Int, Int, Int> =Tuple3<Int, Int, Int>(1, 2, 3)
        |fun bar(): Pair<Int, Int> =Pair<Int, Int>(1, 2)
      """.stripMargin)

  Try
}

