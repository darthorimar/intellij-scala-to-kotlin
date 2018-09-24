package darthorimar.scalaToKotlinConverter

class ApplyConversionTest extends ConverterTestBase {

  def testApplyConversions(): Unit =
    doTest("""val y: Option[Int] = Some(1)
         |val foo: Int => Int = v => v + 1
         |val bar: Int => Int = v => v * 42
         |val res = y.map(foo).map(bar).map(foo)
         |
         |""".stripMargin, """val y: Int? = Some.apply(1)
         |val foo: (Int) -> String = { v -> v.toString() }
         |val res: String? = y.let { foo(it) }""".stripMargin)
}
