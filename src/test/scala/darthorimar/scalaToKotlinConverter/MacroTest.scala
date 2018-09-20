package darthorimar.scalaToKotlinConverter
import darthorimar.scalaToKotlinConverter.ASTDelegatable.delegatable
import junit.framework.TestCase
@delegatable case class Hi(a: Int, b: String) {}

class MacroTest extends TestCase {

  def testMacro(): Unit = {
    val a = Hi(1, "2")
    a.a = 2
    println(a.a)
  }

}
