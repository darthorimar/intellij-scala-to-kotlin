package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast._

import scala.collection.mutable

class KotlinBuilder extends KotlinBuilderBase {
  def gen(ast: AST): Unit =
    ast match {
      case FileDef(pckg, imports, defns) =>
        if (pckg.trim.nonEmpty) {
          str("package ")
          str(pckg)
        }
        if (imports.nonEmpty) {
          nl()
          nl()
          repNl(imports)(gen)
          nl()
          nl()
        }
        repNl(defns)(gen)

      case Defn(attrs, t, name, consruct, supers, block) =>
        rep(attrs, " ")(gen)
        if (attrs.nonEmpty) str(" ")
        genKeyword(t)
        str(" ")
        str(name)
        opt(consruct)(gen)
        if (supers.nonEmpty) {
          str(" : ")
          rep(supers, ", ")(gen)
        }
        str(" ")
        gen(block)

      case Super(ty, construct) =>
        genType(ty, false)

      case EmptyConstruct =>

      case ParamsConstruct(params) =>
        str("(")
        rep(params, ", ")(gen)
        str(")")

      case ConstructParam(parType, mod, name, ty) =>
        gen(mod)
        str(" ")
        gen(parType)
        str(" ")
        str(name)
        genType(ty)

      case ValDef(name, ty, expr) =>
        str("val ")
        str(name)
        genType(ty)
        str(" = ")
        gen(expr)
      case VarDef(name, ty, expr) =>
        str("var ")
        str(name)
        genType(ty)
        str(" = ")
        gen(expr)
      case DefnDef(name, ty, args, retType, body) =>
        str("fun ")
        str(name)
        str("(")
        rep(args, ", ") { case DefParam(ty, name) =>
          str(name)
          genType(ty)
        }
        str(")")
        genType(retType)
        str(" ")
        if (body.isSingle) str("=")
        gen(body)
      case ImportDef(reference, names) =>
        repNl(names) { n =>
          str("import ")
          str(reference)
          str(".")
          str(n)
        }
      case BinExpr(ty, op, left, right) =>
        gen(left)
        str(" ")
        gen(op)
        str(" ")
        gen(right)
      case ParenExpr(inner) =>
        str("(")
        gen(inner)
        str(")")
      case LambdaExpr(ty, params, expr) =>
        str("{ ")
        rep(params, ", ") { case DefParam(ty, name) =>
          str(name)
        }
        str(" -> ")
        gen(expr)
        str("}")
      case AssignExpr(left, right) =>
        gen(left)
        str(" = ")
        gen(right)
      case CallExpr(ty, ref, typeParams, params) =>
        gen(ref)
        if (typeParams.nonEmpty) {
          str("<")
          rep(typeParams, ", ")(gen)
          str(">")
        }
        str("(")
        rep(params, ", ")(gen)
        str(")")
      case IfExpr(ty, cond, trueB, falseB) =>
        str("if (")
        gen(cond)
        str(")")
        gen(trueB)
        if (falseB.stmts.nonEmpty) {
          str(" else ")
          gen(falseB)
        }
      case LitExpr(ty, name) =>
        str(name)
      case UnderScExpr(ty) =>
        str("it")
      case RefExpr(ty, obj, ref) =>
        opt(obj) { x => gen(x); str(".") }
        gen(ref)

      case RefFExpr(ty, name) =>
        str(name)
      case MatchExpr(ty, expr, clauses) =>
        str("when(")
        gen(expr)
        str(") {")
        indent()
        repNl(clauses) { case CaseClause(pattern, expr) =>
          gen(pattern)
          str(" -> ")
          gen(expr)
        }
        str("}")
        unIndent()
      case NewExpr(ty, name, args) =>
        str(name)
        str("(")
        rep(args, ", ")(gen)
        str(")")
      case SingleBlock(stmt) =>
        gen(stmt)
      case MultiBlock(stmts) =>
        str("{")
        indent()
        repNl(stmts)(gen)
        unIndent()
        str("}")
      case EmptyBlock =>
      case BinOp(name) =>
        str(name)
      case LitPattern(lit) =>
        gen(lit)
      case ReferencePattern(ref) =>
        str(ref)
      case WildcardPattern =>
        str("else")
      case ConstructorPattern(ref, args) =>
        str(ref)
        str("(")
        rep(args, ", ")(gen)
        str(")")
      case TypedPattern(ref, ty) => //todo use ref
        str("is ")
        genType(ty, false)
      case TypeParam(ty) =>
        str(ty)
      case CaseAttr =>
        str("data")
      case EmptyAst =>
        str(" EPMTY_AST ")
      case x: Keyword =>
        genKeyword(x)
    }

  def genKeyword(k: Keyword): Unit =
    str(k.name)

  def genType(t: Type, pref: Boolean = true): Unit = {
    if (pref) str(": ")
    str(t.asKotlin)
  }
}
