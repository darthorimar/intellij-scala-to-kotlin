package darthorimar.scalaToKotlinConverter

class MatchConvertTest extends ConverterTestBase {
  def testMatch(): Unit = {
    doTest(
      """
        |trait A
        |case class B(a: A, b: A) extends A
        |case class C(c: Int) extends A
        |
        |def a(x: Any) = x match {
        |  case B(a, B(C(e: Int), C(d))) if e > 3 => e
        |  case B(a, b) => 42
        |  case q: Int if q == 2 => q
        |  case 2 if 1 == 1 => 1
        |  case _ if 1 == 1 => 2
        | }
      """.stripMargin,
      """import convertedFromScala.lib.*
        |fun a(x: Any): Int {
        |    val match = x
        |    data class `B(a, B(C(e Int), C(d)))_data`(public val a: A, public val e: Int, public val d: Int)
        |    data class `B(a, b)_data`(public val a: A, public val b: A)
        |    val `B(a, B(C(e Int), C(d)))` by lazy {
        |        if (match is B) {
        |            val (a, l) = match
        |            if (a is A && l is B) {
        |                val (l1, l2) = l
        |                if (l1 is C && l2 is C) {
        |                    val (e) = l1
        |                    val (d) = l2
        |                    if (e is Int && d is Int) {
        |                        if (e > 3) return@lazy `B(a, B(C(e Int), C(d)))_data`(a, e, d)
        |                    }
        |                }
        |            }
        |        }
        |        return@lazy null
        |    }
        |    val `B(a, b)` by lazy {
        |        if (match is B) {
        |            val (a, b) = match
        |            if (a is A && b is A) {
        |                return@lazy `B(a, b)_data`(a, b)
        |            }
        |        }
        |        return@lazy null
        |    }
        |    return when {
        |        `B(a, B(C(e Int), C(d)))` != null -> {
        |            val (a, e, d) = `B(a, B(C(e Int), C(d)))`
        |            e
        |        }
        |        `B(a, b)` != null -> {
        |            val (a, b) = `B(a, b)`
        |            42
        |        }
        |        match is Int && match == 2 -> {
        |            match
        |        }
        |        match == 2 && 1 == 1 -> {
        |            1
        |        }
        |        1 == 1 -> {
        |            2
        |        }
        |        else -> throw MatchError(match)
        |    }
        |}
        |interface A
        |data class B(val a: A, val b: A) : A {
        |    companion object {
        |        fun apply(a: A, b: A): B = B(a, b)
        |        fun unapply(x: B): B? = x
        |    }
        |}
        |data class C(val c: Int) : A {
        |    companion object {
        |        fun apply(c: Int): C = C(c)
        |        fun unapply(x: C): C? = x
        |    }
        |}""".stripMargin)
  }


  def testOuterUnapplyInMatch(): Unit =
    doTest(
      """trait I
        |  case class A(i: I) extends I
        |  case class B(x: Int) extends I
        |  object O {
        |    def unapply(arg: Int): Option[A] = Some(A(B(1)))
        |  }
        |  val q = 1 match {
        |    case O(A(B(x))) => x
        |  }""".stripMargin,
      """val q: Int = run {
        |   val match = 1
        |  data class `O(A(B(x)))_data`(public val x: Int)
        |  val `O(A(B(x)))` by lazy {
        |     val l = O.unapply(match)
        |    if (l != null && l is A) {
        |       val (l1) = l
        |      if (l1 is A) {
        |         val (l2) = l1
        |        if (l2 is B) {
        |           val (x) = l2
        |          if (x is Int) return@lazy `O(A(B(x)))_data`(x)
        |        }
        |      }
        |    }
        |    return@lazy null
        |  }
        |  when {
        |    `O(A(B(x)))` != null -> {
        |       val (x) = `O(A(B(x)))`
        |      x
        |    }
        |    else -> throw Exception("Match exception")
        |  }
        |}
        |interface I
        |data class A( val i: I) : I {
        |  companion object  {
        |    fun apply(i: I): A =A(i)
        |    fun unapply(x: A): A? =x
        |  }
        |}
        |data class B( val x: Int) : I {
        |  companion object  {
        |    fun apply(x: Int): B =B(x)
        |    fun unapply(x: B): B? =x
        |  }
        |}
        |object O {
        |  fun unapply(arg: Int): A? =A.apply(B.apply(1))
        |}""".stripMargin)

  def testInnerUnapplyInMatch(): Unit =
    doTest(
      """trait I
        |  case class A(i: I) extends I
        |  case class B(x: Int) extends I
        |  object O {
        |    def unapply(arg: Int): Option[A] = Some(A(B(1)))
        |  }
        |  val q = Right(1) match {
        |    case Right(O(A(B(x)))) => x
        |  }""".stripMargin,
      """ import convertedFromScala.lib.*
        |val q: Int = run {
        |    val match = Right.apply(1)
        |    data class `Right(O(A(B(x))))_data`(public val x: Int)
        |    val `Right(O(A(B(x))))` by lazy {
        |        if (match is scala.util.Right) {
        |            val (l) = match
        |            run {
        |                val (l1) = O.unapply(l) ?: return@lazy null
        |                if (l1 is A) {
        |                    val (l2) = l1
        |                    if (l2 is B) {
        |                        val (x) = l2
        |                        if (x is Int) {
        |                            return@lazy `Right(O(A(B(x))))_data`(x)
        |                        }
        |                    }
        |                }
        |            }
        |        }
        |        return@lazy null
        |    }
        |    when {
        |        `Right(O(A(B(x))))` != null -> {
        |            val (x) = `Right(O(A(B(x))))`
        |            x
        |        }
        |        else -> throw MatchError(match)
        |    }
        |}
        |interface I
        |data class A(val i: I) : I {
        |    companion object {
        |        fun apply(i: I): A = A(i)
        |        fun unapply(x: A): A? = x
        |    }
        |}
        |data class B(val x: Int) : I {
        |    companion object {
        |        fun apply(x: Int): B = B(x)
        |        fun unapply(x: B): B? = x
        |    }
        |}
        |object O {
        |    fun unapply(arg: Int): A? = A.apply(B.apply(1))
        |}""".stripMargin)


  def testSomeInWhen(): Unit =
    doExprTest(
      """Some(1) match {
        |   case Some(x) => x + 1
        |   case None => 0
        |}""".stripMargin,
      """
        |    run {
        |        val match = 1
        |        data class `Some(x)_data`(public val x: Int)
        |        val `Some(x)` by lazy {
        |            if (match != null) {
        |                val (x) = match
        |                if (x is Int) {
        |                    return@lazy `Some(x)_data`(x)
        |                }
        |            }
        |            return@lazy null
        |        }
        |        when {
        |            `Some(x)` != null -> {
        |                val (x) = `Some(x)`
        |                x + 1
        |            }
        |            match == null -> {
        |                0
        |            }
        |            else -> throw MatchError(match)
        |        }
        |    }
        |""".stripMargin)

}
