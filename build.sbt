name := "untitled"

version := "0.1"

scalaVersion := "2.12.6"

ideaPluginName in ThisBuild := "IntellijScalaToKotlin"

ideaExternalPlugins += IdeaPlugin.Zip("Scala", url("https://plugins.jetbrains.com/files/1347/48884/scala-intellij-bin-2018.2.10.zip?updateId=48884&pluginId=1347"))
