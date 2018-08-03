package org.jetbrains.plugins.kotlinConverter.scopes


import *
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal
import Int
import U
import Unit
import List
import R
import T
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.Companion.SettedScopedVal

open class ScopedVal<T>(public val initial: T) {
    private var stack: List<T> = listOf(initial) + emptyList()
    fun set(value: T): SettedScopedVal<T> = SettedScopedVal<T>(value, this)
    fun get(): T = stack.first()
    fun updated(update: (T) -> T): SettedScopedVal<T> = set(update.apply { get(it) })
    fun <R> call(func: (T) -> R): R = func.apply { get(it) }

    companion object {
        fun <T> scoped(vararg vals: SettedScopedVal<*>, body: T): T {
            vals.forEach(it.set())
            return try {
                body
            } finally {
                vals.forEach(it.unset())
            }
        }

        open class SettedScopedVal<T>(public val value: T, public val scopedVal: ScopedVal<T>) {
            private fun unset(): Unit {
                scopedVal.stack = scopedVal.stack.drop(1)
            }

            private fun set(): Unit {
                scopedVal.stack = listOf(value) + scopedVal.stack
            }
        }

        fun <T> implicitGet(scopedVal: ScopedVal<T>): T = scopedVal.get()
    }
}