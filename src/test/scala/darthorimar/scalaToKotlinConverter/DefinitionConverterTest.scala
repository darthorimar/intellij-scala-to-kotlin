package darthorimar.scalaToKotlinConverter

class DefinitionConverterTest extends ConverterTestBase {

  case class A(i: Int, b: String)

  def testCaseClassDef(): Unit =
    doTest(
      """case class A(i: Int, b: String)
      """.stripMargin,
      """|data class A(val i: Int, val b: String) {
         |  companion object {
         |        fun apply(i: Int, b: String): A = A(i, b)
         |        fun unapply(x: A): A? = x
         |    }
         |}
      """.stripMargin
    )

  def testTraitDef(): Unit =
    doTest(
      """
        |class A(a: Int)
        |trait B
        |class C extends A(1) with B
      """.stripMargin,
      """
        |open class A(private val a: Int)
        |interface B
        |open class C() : A(1), B
      """.stripMargin
    )

  def companionObjectTest(): Unit =
    doTest(
      """class A {
        |}
        |object A {def a = 5}
        |object B
      """.stripMargin,
      """open class A() {
        |  companion object {
        |    public fun a(): Int =5
        |  }
        |}
        |object B
        |
        |}""".stripMargin
    )

  def testMultipleConstctorParams(): Unit =
    doTest(
      """
        |class A(a: Int, b: String)
        |class C extends A(1, "nya")
      """.stripMargin,
      """open class A(private val a: Int, private val b: String)
        |open class C() : A(1, "nya")
      """.stripMargin
    )

  def testClassModifiers(): Unit =
    doTest(
      """
        |final class A
        |class B
        |abstract class C
      """.stripMargin,
      """class A()
        |open class B()
        |abstract class C()
      """.stripMargin
    )

  def testImplicitClass(): Unit =
    doTest(
      """
        | implicit class IntOps(val i: Int) {
        |    def plusOne = i + 1
        |    def minusOne = i - 1
        |  }
        |  def foo = 1.plusOne.minusOne
      """.stripMargin,
      """fun foo(): Int = 1.plusOne().minusOne()
        |fun Int.plusOne(): Int = this + 1
        |fun Int.minusOne(): Int = this - 1
      """.stripMargin
    )

  def testImplicitClassTypeParams(): Unit =
    doTest(
      """
        | implicit class ListOps[T](val list: List[T]) {
        |    def cast[K]: List[K] = list.asInstanceOf[List[K]]
        |  }
        |  def foo = List(1).cast[Double]
      """.stripMargin,
      """fun foo(): List<Double> = listOf(1).cast<Double>()
        |fun <T, K> List<T>.cast(): List<K> = this as List<K>
      """.stripMargin
    )
}
