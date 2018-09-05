public inline fun <T, R : Any> List<T>.collect(f: (T) -> R): List<R> =
        this.mapNotNull {
            try {
                f(it)
            } catch (_: MatchError) {
                null
            }
        }

public inline fun <T, reified R : Any> Array<T>.collect(f: (T) -> R): Array<R> =
        this.mapNotNull {
            try {
                f(it)
            } catch (_: MatchError) {
                null
            }
        }.toTypedArray()

