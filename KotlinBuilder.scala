package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.ast.Stmt._
import org.jetbrains.plugins.kotlinConverter.ast.Expr._

class KotlinBuilder extends KotlinBuilderBase {
  def gen(ast: AST): Unit =  ast match {
    case FileDef(pckg, imports, defns) =>
      repNl(imports)(gen)
      nl()
      repNl(defns)(gen)
    case ClassDef(name, block) =>
      str("class ")
      str(name)
      str(" ")
      gen(block)

    case ValDef(name, ty) =>
    case DefnDef(name, ty, args, body) =>
      str("fun ")
      str(name)
      str("(")
      rep(args, ", ") { case DefParam(ty, name) =>
        str(name)
        str(": ")
        gen(ty)
      }
      str("): ")
      gen(ty)
      str(" ")
      gen(body)
    case ObjDef(name, members) =>
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
    case Call(ty, ref, typeParams, params) =>
      gen(ref)
      str("(")
      rep(params, ", ")(gen)
      str(")")
    case Lit(ty, name) =>
      str(name)
    case Ref(ty, name) =>
      str(name)
    case Match(expr, clauses) =>
      str("when(")
      gen(expr)
      str(") {")
      indent()
      repNl(clauses) { case CaseClause(pattern, expr) =>
          gen(pattern)
          str(" -> ")
          gen(expr)
      }
      unIndent()
    case SingleBlock(stmt) =>
      gen(stmt)
    case MultiBlock(stmts) =>
      str("{")
      indent()
      repNl(stmts)(gen)
      unIndent()
      str("}")
    case EmptyBlock =>
    case Type(name) =>
      str(name)
    case BinOp(name) =>
      str(name)
    case LitPattern(lit) =>
      gen(lit)
    case WildcardPattern =>
      str("else")
  }
}
