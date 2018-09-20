package darthorimar.scalaToKotlinConverter

class ApplyConversionTest extends ConverterTestBase {

  def testApplyConversions(): Unit =
    doTest("""val y: Option[Int] = Some(1)
         |val foo: Int => String = v => v.toString
         |val res = y.map(foo)
         |
         |""".stripMargin, """val y: Int? = Some.apply(1)
         |val foo: (Int) -> String = { v -> v.toString() }
         |val res: String? = y.let { foo(it) }""".stripMargin)
}
