class MatchError(obj: Any?) : RuntimeException() {
    private val objString by lazy {
        fun ofClass() = "of class " + obj?.javaClass?.name
        if (obj == null) "null"
        else try {
            obj.toString() + " (" + ofClass() + ")"
        } catch (_: Throwable) {
            "an instance " + ofClass()
        }
    }
    override val message: String?
        get() = objString
}