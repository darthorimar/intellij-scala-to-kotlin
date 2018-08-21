public inline fun <T, R : Any> List<T>.collect(f: (T) -> R): List<R> =
        this.mapNotNull {
            try {
                f(it)
            } catch (_: MatchError) {
                null
            }
        }
