import com.diffplug.gradle.spotless.SpotlessTask

version = "0.1.1"

plugins {
    id("org.jetbrains.intellij") version "0.3.6"
    id("scala")
    id("com.diffplug.gradle.spotless") version "3.14.0"

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
    testCompile("org.scala-lang:scala-library:2.12.6")
    testCompile("junit:junit:4.12")
}

spotless {
    isEnforceCheck = false
    scala {
        scalafmt().configFile("scalafmt.conf")
    }
}