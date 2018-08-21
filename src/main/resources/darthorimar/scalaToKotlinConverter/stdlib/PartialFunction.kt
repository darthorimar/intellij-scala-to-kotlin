interface PartialFunction<in A, out B> {
    fun isDefinedAt(x: A): Boolean =
            try {
                invoke(x)
                true
            } catch (e: MatchError) {
                false
            }

    fun invoke(x: A): B
}