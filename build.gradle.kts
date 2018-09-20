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

val scalaCompiler by configurations.creating



project(":") {
    intellij {
        pluginName = "IntellijScalaToKotlin"
    }
    scalaCompiler.isTransitive = false

    dependencies {
        compile("org.scala-lang:scala-library:2.12.6")
//        testCompile("org.scala-lang:scala-library:2.12.6")
        testCompile("junit:junit:4.12")
        compile(project(":converter-api"))
        compile(group = "net.jcazevedo", name = "moultingyaml_2.12", version = "0.4.0")
        compile(files("/home/ilya/code/Meerkat/core/target/scala-2.12/Meerkat-assembly-0.1.0.jar"))
        compile(group = "guru.nidi", name = "graphviz-java", version = "0.7.0")

        scalaCompiler(group = "org.scalamacros", name = "paradise_2.12.6", version = "2.1.1")
    }
    val scalaCompilerOptions = "-Xplugin:${scalaCompiler.singleFile.path}"
    tasks.withType<ScalaCompile> {
        scalaCompileOptions.additionalParameters = listOf(scalaCompilerOptions)
    }
}




project(":converter-api") {

}