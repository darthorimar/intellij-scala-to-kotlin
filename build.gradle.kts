plugins {
    idea
    kotlin("jvm") version "1.2.61"
    id("org.jetbrains.intellij") version "0.3.6"
    id("scala")
    id("com.diffplug.gradle.spotless") version "3.14.0"
}

allprojects {
    apply {
        plugin("idea")
        plugin("kotlin")
        plugin("org.jetbrains.intellij")
    }

    repositories {
        mavenCentral()
    }


    intellij {
        version = "IC-2018.2"
        setPlugins("Kotlin", "org.intellij.scala:2018.2.10")
    }
    version = "0.1.1"
}




spotless {
    isEnforceCheck = false
    scala {
        scalafmt().configFile("scalafmt.conf")
    }
}

project(":") {
    intellij {
        pluginName = "IntellijScalaToKotlin"
    }
    dependencies {
        compileOnly("org.scala-lang:scala-library:2.12.6")
        testCompile("org.scala-lang:scala-library:2.12.6")
        testCompile("junit:junit:4.12")
        compile(project(":converter-api"))
        compile(group = "net.jcazevedo", name = "moultingyaml_2.12", version = "0.4.0")    }
}

project(":converter-api") {

}