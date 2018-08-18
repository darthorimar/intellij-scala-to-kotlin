sealed class Either<out A, out B> {
    abstract fun isLeft(): Boolean
    abstract fun isRight(): Boolean
    abstract fun toOption(): B?
    abstract fun <B1> map(f: (B) -> B1): Either<A, B1>
    abstract fun <A1, B1> flatMap(f: (B) -> Either<A1, B1>): Either<A1, B1>
}

class Left<out A, out B>(public val value: A) : Either<A, B>() {
    override fun isLeft(): Boolean = true
    override fun isRight(): Boolean = false
    override fun toOption(): B? = null
    override fun <B1> map(f: (B) -> B1): Either<A, B1> = this as Either<A, B1>
    override  fun <A1, B1> flatMap(f: (B) -> Either<A1, B1>): Either<A1, B1> =
            this as Either<A1, B1>
}