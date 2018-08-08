package org.jetbrains.plugins.kotlinConverter.builder.codegen

import org.jetbrains.plugins.kotlinConverter.builder.BuilderBase

trait Definition extends BuilderBase {
  protected def generate(): Unit
  def get: String = {
    generate()
    text
  }
  def name: String
}
