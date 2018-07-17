package org.jetbrains.plugins.kotlinConverter.scopes

import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.SettedScopedVal

class ScopedVal[T](initial: T) {
  private var stack: List[T] = initial :: Nil

  def set(value: T): SettedScopedVal[T] =
    new SettedScopedVal[T](value, this)

  def get: T = stack.head

  def updated(update: T => T): SettedScopedVal[T] =
    set(update(get))

  def call[R](func: T => R): R =
    func(get)
}

object ScopedVal {
  def scoped[T](vals: SettedScopedVal[_]*)(body: => T): T = {
    vals.foreach(_.set())
    try body
    finally vals.foreach(_.unset())
  }

  class SettedScopedVal[T](value: T, scopedVal: ScopedVal[T]) {
    private[ScopedVal] def unset(): Unit = {
      scopedVal.stack = scopedVal.stack.tail
    }

    private[ScopedVal] def set(): Unit = {
      scopedVal.stack = value :: scopedVal.stack
    }
  }

  implicit def implicitGet[T](scopedVal: ScopedVal[T]): T =
    scopedVal.get

}

