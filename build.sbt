ideaPluginName in ThisBuild := "IntellijScalaToKotlin"

lazy val scalaToKotlin = (project in file("."))
  .settings(Seq(
    name := "scala-to-kotlin",
    version := "0.1",
    ideaInternalPlugins := Seq("Kotlin"),
    ideaExternalPlugins += IdeaPlugin.Zip("Scala", url("https://plugins.jetbrains.com/files/1347/48884/scala-intellij-bin-2018.2.10.zip?updateId=48884&pluginId=1347"))
  ))

lazy val ideaRunner = (project in file("idea-runner"))
  .settings(
    dumpDependencyStructure := null, // avoid cyclic dependencies on products task
    products := packagePlugin.in(scalaToKotlin).value :: Nil,
    packageMethod := org.jetbrains.sbtidea.Keys.PackagingMethod.Skip(),
    unmanagedJars in Compile := ideaMainJars.value,
    unmanagedJars in Compile += file(System.getProperty("java.home")).getParentFile / "lib" / "tools.jar"
  )
  .dependsOn(scalaToKotlin)
