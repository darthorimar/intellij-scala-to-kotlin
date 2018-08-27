package darthorimar.scalaToKotlinConverter

class RealConverterTest extends ConverterTestBase {
  def testScopedVal(): Unit =
    doTest(
      """package darthorimar.scalaToKotlinConverter.scopes
        |
        |import darthorimar.scalaToKotlinConverter.scopes.ScopedVal.SettedScopedVal
        |
        |class ScopedVal[T](initial: T) {
        |  private var stack: List[T] = initial :: Nil
        |
        |  def set(value: T): SettedScopedVal[T] =
        |    new SettedScopedVal[T](value, this)
        |
        |  def get: T = stack.head
        |
        |  def updated(update: T => T): SettedScopedVal[T] =
        |    set(update.apply(get))
        |
        |  def call[R](func: T => R): R =
        |    func.apply(get)
        |}
        |
        |object ScopedVal {
        |  def scoped[T](vals: SettedScopedVal[_]*)(body: => T): T = {
        |    vals.foreach(_.set())
        |    try body
        |    finally vals.foreach(_.unset())
        |  }
        |
        |  class SettedScopedVal[T](value: T, scopedVal: ScopedVal[T]) {
        |    private[ScopedVal] def unset(): Unit = {
        |      scopedVal.stack = scopedVal.stack.tail
        |    }
        |
        |    private[ScopedVal] def set(): Unit = {
        |      scopedVal.stack = value :: scopedVal.stack
        |    }
        |  }
        |
        |  implicit def implicitGet[T](scopedVal: ScopedVal[T]): T =
        |    scopedVal.get
        |}
        |
        |
      """.stripMargin,
      """package darthorimar.scalaToKotlinConverter.scopes
        |open class ScopedVal<T>(private val initial: T) {
        |    private var stack: List<T> = listOf(initial) + emptyList()
        |    fun set(value: T): SettedScopedVal<T> = SettedScopedVal<T>(value, this)
        |    fun get(): T = stack.first()
        |    fun updated(update: (T) -> T): SettedScopedVal<T> = set(update(get()))
        |    fun <R> call(func: (T) -> R): R = func(get())
        |    companion object {
        |        fun <T> scoped(vararg vals: SettedScopedVal<*>, body: () -> T): T {
        |            vals.forEach { it.set() }
        |            return try {
        |                body()
        |            } finally {
        |                vals.forEach { it.unset() }
        |            }
        |        }
        |        open class SettedScopedVal<T>(private val value: T, private val scopedVal: ScopedVal<T>) {
        |            internal fun unset(): Unit {
        |                scopedVal.stack = scopedVal.stack.drop(1)
        |            }
        |            internal fun set(): Unit {
        |                scopedVal.stack = listOf(value) + scopedVal.stack
        |            }
        |        }
        |        fun <T> implicitGet(scopedVal: ScopedVal<T>): T = scopedVal.get()
        |    }
        |}""".stripMargin)
}
