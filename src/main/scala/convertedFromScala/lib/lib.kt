package convertedFromScala.lib

data class Tuple2<out T1, out T2>(val _1: T1, val _2: T2)

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