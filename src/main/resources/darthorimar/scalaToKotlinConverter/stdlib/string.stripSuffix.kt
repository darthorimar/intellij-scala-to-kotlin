package darthorimar.scalaToKotlinConverter.stdlib

fun String.stripSuffix(suffix: String): String =
        if (this.endsWith(suffix)) this.substring(0, this.length - suffix.length)
        else this
