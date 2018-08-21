version = "0.1"

plugins {
    id("org.jetbrains.intellij") version "0.3.6"
    id("scala")
}


intellij {
    version = "IC-2018.2"
    pluginName = "IntellijScalaToKotlin"
    setPlugins("Kotlin", "org.intellij.scala:2018.2.10")
}

repositories {
    mavenCentral()
}

dependencies {
    compileOnly("org.scala-lang:scala-library:2.12.6")
}
