package org.jetbrains.plugins.kotlinConverterpublic

abstract class KotlinBuilderBase {
    val builder = StringBuffer()
    var i: Int = 0
    val indentStep = 2
    public fun str(v: Any): Unit = builder.append(v)
    public fun nl(): Unit {
        str("\n")
        str(" ".repeat(i))
    }

    public fun indent(): Unit {
        i += indentStep
        nl()
    }

    public fun unIndent(): Unit {
        i -= indentStep
        nl()
    }

    public fun rep(values: List<T>, sep: Unit, h: (T) -> Unit): Unit = if (values.isNotEmpty()) {
        values.init().forEach { value ->
            h(value)
            sep
        }
        h { values.last(it) }
    }

    public fun rep(values: List<T>, sep: String, h: (T) -> Unit): Unit = rep()(values, str(sep), h)
    public fun repNl(values: List<T>, h: (T) -> Unit): Unit = rep()(values, nl(), h)
    public fun opt(value: T?, h: (T) -> Unit): Unit = value.foreach(h)
    public fun text(): String = builder.toString
}