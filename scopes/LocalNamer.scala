package org.jetbrains.plugins.kotlinConverter.scopes

import scala.collection.mutable

class LocalNamer {
  val prefixes = mutable.Map.empty[String, Int]
  def newName(name: String): String =
    prefixes.get(name)  match {
      case Some(index) =>
        prefixes(name)+=1
        s"$name$index"
      case None =>
        prefixes(name) = 1
        name
    }
}
