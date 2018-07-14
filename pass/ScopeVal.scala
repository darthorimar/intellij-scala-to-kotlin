package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.pass.ScopeVal.SettedScopedVal

class ScopeVal[T](initial: T) {
  private var stack: List[T] = initial :: Nil

  def set(value: T): SettedScopedVal[T] = {
    stack = value :: stack
    new SettedScopedVal[T](stack.tail, this)
  }

  def get: T = stack.head

  def updated(update: T => T): SettedScopedVal[T] =
    set(update(get))

  def call[R](func: T => R): R =
    func(get)

}

object ScopeVal {
  def scoped[T](vals: SettedScopedVal[_]*)(body: => T): T = {
    try body
    finally vals.foreach(_.unset())
  }

  class SettedScopedVal[T](stack: List[T], scopeVal: ScopeVal[T]) {
    def unset(): Unit = {
      scopeVal.stack = stack
    }
  }

}

