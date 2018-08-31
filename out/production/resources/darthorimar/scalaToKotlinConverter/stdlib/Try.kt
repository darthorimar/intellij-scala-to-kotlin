sealed class Try<out T> {
    abstract fun isSuccess(): Boolean
    abstract fun isFailure(): Boolean
    abstract fun getOrElse(default: () -> @UnsafeVariance T): T
    abstract fun orElse(default: () -> Try<@UnsafeVariance T>): Try<T>
    abstract fun get(): T
    abstract fun <U> foreach(f: (T) -> U)
    abstract fun <U> flatMap(f: (T) -> Try<U>): Try<U>
    abstract fun <U> map(f: (T) -> U): Try<U>
    abstract fun filter(p: (T) -> Boolean): Try<T>
    abstract fun toOption(): T?
    abstract fun <U> recover(f: (Throwable) -> U): Try<U>
    abstract fun <U> recoverWith(f: (Throwable) -> Try<U>): Try<U>
}

fun <T> runTry(block: () -> T): Try<T> =
        try {
            Success(block())
        } catch (e: Exception) {
            Failure(e)
        }

class Failure<out T>(val exception: Throwable) : Try<T>() {
    override fun isSuccess(): Boolean = false
    override fun isFailure(): Boolean = true
    override fun getOrElse(default: () -> @UnsafeVariance T): T = default()

    override fun orElse(default: () -> Try<@UnsafeVariance T>): Try<T> =
            try {
                default()
            } catch (e: Throwable) {
                Failure(e)
            }

    override fun get(): T = throw exception
    override fun <U> foreach(f: (T) -> U) {}
    override fun <U> flatMap(f: (T) -> Try<U>): Try<U> = this as Try<U>
    override fun <U> map(f: (T) -> U): Try<U> = this as Try<U>
    override fun filter(p: (T) -> Boolean): Try<T> = this
    override fun toOption(): T? = null
    override fun <U> recover(f: (Throwable) -> U): Try<U> =
            try {
                Success(f(exception))
            } catch (e: MatchError) {
                this as Try<U>
            } catch (e: Throwable) {
                Failure(e)
            }

    override fun <U> recoverWith(f: (Throwable) -> Try<U>): Try<U> =
            try {
                f(exception)
            } catch (e: MatchError) {
                this as Try<U>
            } catch (e: Throwable) {
                Failure(e)
            }
}

class Success<out T>(public val value: T) : Try<T>() {
    override fun isSuccess(): Boolean = true
    override fun isFailure(): Boolean = false
    override fun getOrElse(default: () -> @UnsafeVariance T): T = get()
    override fun orElse(default: () -> Try<@UnsafeVariance T>): Try<T> = this
    override fun get(): T = value
    override fun <U> foreach(f: (T) -> U) {
        f(value)
    }

    override fun <U> flatMap(f: (T) -> Try<U>): Try<U> =
            try {
                f(value)
            } catch (e: Throwable) {
                Failure(e)
            }

    override fun <U> map(f: (T) -> U): Try<U> = runTry { f(value) }
    override fun filter(p: (T) -> Boolean): Try<T> =
            if (p(value)) this
            else Failure(NoSuchElementException("Predicate does not hold for $value"))

    override fun toOption(): T? = value

    override fun <U> recover(f: (Throwable) -> U): Try<U> = this as Try<U>
    override fun <U> recoverWith(f: (Throwable) -> Try<U>): Try<U> = this as Try<U>
}