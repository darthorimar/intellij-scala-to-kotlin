package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.ast.Stmt._
import org.jetbrains.plugins.kotlinConverter.ast.Expr._

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
      case ClassDef(attrs, name, consruct, supers, block) =>
        rep(attrs, " ") (gen)
        if (attrs.nonEmpty) str(" ")
        str("class ")
        str(name)
        gen(consruct)
        str(" ")
        gen(block)
      case EmptyConstruct =>

      case ParamsConstruct(params) =>
        str("(")
        rep(params, ", ") (gen)
        str(")")

      case ConstructParam(parType, mod, name, ty) =>
        gen(mod)
        str(" ")
        gen(parType)
        str(" ")
        str(name)
        genType(ty)

      case ValType =>
        str("val")
      case VarType =>
        str("var")
      case NoType =>
      case PrivModifier =>
        str("private")
      case PublModifier =>
        str("public")
      case NoModifier =>

      case TraitDef(name, supers, block) =>
        str("trait ")
        str(name)
        str(" ")
        gen(block)

      case ObjDef(name, supers, block) =>
        str("object ")
        str(name)
        str(" ")
        gen(block)

      case ValDef(name, ty, expr) =>
        str("val ")
        str(name)
        genRealType(ty)
        str(" = ")
        gen(expr)
      case VarDef(name, ty, expr) =>
        str("var ")
        str(name)
        genRealType(ty)
        str(" = ")
        gen(expr)
      case DefnDef(name, ty, args, body) =>
        str("fun ")
        str(name)
        str("(")
        rep(args, ", ") { case DefParam(ty, name) =>
          str(name)
          genType(ty)
        }
        str(")")
        genType(ty)
        str(" ")
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
      case Lambda(params, expr) =>
        str("{ ")
        rep(params, ", ") { case DefParam(ty, name) =>
          str(name)
        }
        str(" -> ")
        gen(expr)
        str("}")
      case Assign(left, right) =>
        gen(left)
        str(" = ")
        gen(right)
      case Call(ty, ref, typeParams, params) =>
        gen(ref)
        if (typeParams.nonEmpty) {
          str("<")
          rep(typeParams, ", ")(gen)
          str(">")
        }
        str("(")
        rep(params, ", ")(gen)
        str(")")
      case Lit(ty, name) =>
        str(name)
      case UnderSc =>
        str("it")
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
      case New(name, args) =>
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
        genType(ty)
      case TypeParam(ty) =>
        str(ty)
      case CaseAttr =>
        str("data")
      case EmptyAst =>
        str(" EPMTY_AST ")
    }

  def genRealType(ty: Type): Unit =
    ty.real.foreach { c =>
      str(": ")
      str(c)
    }

  def genType(ty: Type): Unit = {
    str(": ")
    str(ty.realOfInf.getOrElse("Any"))
  }
}
