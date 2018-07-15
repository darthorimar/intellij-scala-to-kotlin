package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast. _

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
        opt(block)(gen)

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

      case ValDef(destructors, expr) =>
        str("val ")
        if (destructors.size == 1) {
          gen(destructors.head)
        } else {
          str("(")
          rep(destructors, ", ")(gen)
          str(")")
        }
        str(" = ")
        gen(expr)
      case LazyValDef(name, ty, expr) =>
        str("val ")
        str(name)
        str(" by lazy ")
        gen(expr)
      case VarDef(name, ty, expr) =>
        str("var ")
        str(name)
        genType(ty)
        str(" = ")
        gen(expr)

      case ReturnExpr(label, expr) =>
        str("return")
        opt(label) { l =>
          str("@")
          str(l)
        }
        str(" ")
        opt(expr)(gen)

      case p: MatchCasePattern =>
        str(p.name)

      case DefnDef(attrs, name, ty, args, retType, body) =>
        rep(attrs, " ")(gen)
        if (attrs.nonEmpty) str(" ")
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
        opt(body){ b =>
          if (!b.isInstanceOf[BlockExpr]) str("=")
          gen(b)
        }

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
      case LambdaExpr(ty, params, expr, needBraces) =>
        str("{ ")
        if (needBraces) str("(")
        rep(params, ", ") { case DefParam(ty, name) =>
          str(name)
        }
        if (needBraces) str(")")
        if (params.nonEmpty) str(" -> ")
        expr match {
          case b: BlockExpr =>
            repNl(b.exprs)(gen)
          case _ =>
            gen(expr)
        }
        str(" }")
      case AssignExpr(left, right) =>
        gen(left)
        str(" = ")
        gen(right)

      case TypeExpr(ty) =>
        genType(ty, false)

      case CallExpr(ty, ref, params) =>
        gen(ref)
        if (params.size == 1 && params.head.isInstanceOf[LambdaExpr]) {
          str(" ")
          gen(params.head)
        } else {
          str("(")
          rep(params, ", ")(gen)
          str(")")
        }

      case RefExpr(ty, obj, ref, typeParams, isFunc) =>
        opt(obj) { x => gen(x); str(".") }
        str(ref)
        if (typeParams.nonEmpty) {
          str("<")
          rep(typeParams, ", ")(gen)
          str(">")
        }

      case IfExpr(ty, cond, trueB, falseB) =>
        str("if (")
        gen(cond)
        str(")")
        gen(trueB)
        opt(falseB) { b =>
          str(" else ")
          gen(b)
        }
      case PostExpr(ty, obj, op) =>
        gen(obj)
        str(op)

      case LitExpr(ty, name) =>
        str(name)
      case UnderScExpr(ty) =>
        str("it")

      case WhenExpr(ty, expr, clauses) =>
        str("when")
        opt(expr) { e =>
          str(" (")
          gen(e)
          str(")")
        }
        str(" {")
        indent()
        repNl(clauses) {
          case ExprWhenClause(clause, expr) =>
            gen(clause)
            str(" -> ")
            gen(expr)
          case ElseWhenClause(expr) =>
            str("else -> ")
            gen(expr)
        }

        str("}")
        unIndent()
      case NewExpr(ty, name, args) =>
        str(name)
        str("(")
        rep(args, ", ")(gen)
        str(")")

      case BlockExpr(stmts) =>
        str("{")
        indent()
        repNl(stmts)(gen)
        unIndent()
        str("}")

      case BinOp(name) =>
        str(name)
      case TypeParam(ty) =>
        genType(ty, false)
      case CaseAttr =>
        str("data")
      //      case EmptyAst =>
      //        str(" EPMTY_AST ")
      case x: Keyword =>
        genKeyword(x)
      case _ =>
    }

  def genKeyword(k: Keyword): Unit =
    str(k.name)

  def genType(t: Type, pref: Boolean = true): Unit = {
    if (pref) str(": ")
    str(t.asKotlin)
  }
}
