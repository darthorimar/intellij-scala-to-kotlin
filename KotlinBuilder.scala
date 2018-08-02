package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.scopes.{BuilderState, ScopedVal}

import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped

class KotlinBuilder extends KotlinBuilderBase {
  val stateVal: ScopedVal[BuilderState] = new ScopedVal[BuilderState](BuilderState())

  def gen(ast: AST): Unit =
    ast match {
      case _: ErrorAst =>
        str("/* ERROR */")

      case FileDef(pckg, imports, defns) =>
        if (pckg.trim.nonEmpty) {
          str("package ")
          str(pckg)
          nl()
        }
        if (imports.nonEmpty) {
          nl()
          nl()
          repNl(imports)(gen)
          nl()
          nl()
        }
        repNl(defns)(gen)

      case Defn(attrs, t, name, typeParams, consruct, supersBlock, block) =>
        rep(attrs, " ")(gen)
        if (attrs.nonEmpty) str(" ")
        genKeyword(t)
        str(" ")
        str(name)
        if (typeParams.nonEmpty) {
          str("<")
          rep(typeParams, ", ")(gen)
          str(">")
        }
        opt(consruct)(gen)
        opt(supersBlock) { case SupersBlock(constuctor, supers) =>
          str(" : ")
          opt(constuctor) { case SuperConstructor(exprType, exprs) =>
            genType(exprType, false)
            str("(")
            rep(exprs, ", ")(gen)
            str(")")
          }
          if (constuctor.isDefined && supers.nonEmpty) {
            str(", ")
          }
          rep(supers, ", ")(genType(_, false))
        }
        str(" ")
        opt(block)(gen)


      case EmptyConstructor =>

      case ParamsConstructor(params) =>
        str("(")
        rep(params, ", ")(gen)
        str(")")

      case ConstructorParam(parType, mod, name, exprType) =>
        gen(mod)
        str(" ")
        gen(parType)
        str(" ")
        str(name)
        genType(exprType)

      case x@SimpleValOrVarDef(attributes, isVal, name, valType, expr) =>
        rep(attributes, " ")(gen)
        str(" ")
        str(x.keyword)
        str(" ")
        str(name)
        opt(valType)(genType(_))
        opt(expr) { e =>
          str(" = ")
          gen(e)
        }

      case x@ValOrVarDef(attributes, isVal, patterns, expr) =>
        rep(attributes, " ")(gen)
        str(" ")
        str(x.keyword)
        str(" (")
        rep(patterns, ", ")(gen)
        str(")")
        opt(expr) { e =>
          str(" = ")
          gen(e)
        }

      case LazyValDef(name, ty, expr) =>
        str("val ")
        str(name)
        str(" by lazy ")
        gen(expr)


      case ReturnExpr(label, expr) =>
        str("return")
        opt(label) { l =>
          str("@")
          str(l)
        }
        str(" ")
        opt(expr)(gen)

      case p: CasePattern =>
        str(p.name)

      case DefnDef(attrs, name, typeParams, args, retType, body) =>
        rep(attrs, " ")(gen)
        if (attrs.nonEmpty) str(" ")
        str("fun")
        if (typeParams.nonEmpty) {
          str("<")
          rep(typeParams, ", ")(gen)
          str(">")
        }
        str(" ")
        str(name)
        str("(")
        rep(args, ", ") { case DefParameter(parameterType, name) =>
          str(name)
          genType(parameterType)
        }
        str(")")
        genType(retType)
        str(" ")
        opt(body) { b =>
          if (!b.isInstanceOf[BlockExpr]) str("=")
          gen(b)
        }

      case KotlinTryExpr(exprType, tryBlock, catches, finallyBlock) =>
        str("try ")
        genAsBlock(tryBlock)
        rep(catches, " ") { case KotlinCatchCase(name, valueType, expr) =>
          str("catch (")
          str(name)
          genType(valueType)
          str(") ")
          genAsBlock(expr)
        }
        opt(finallyBlock) { f =>
          str(" finally ")
          genAsBlock(f)
        }

      case ImportDef(reference, names) =>
        if (names.nonEmpty) {
          repNl(names) { n =>
            str("import ")
            str(reference)
            str(".")
            str(n)
          }
        } else {
          str("import ")
          str(reference)
        }
      case InfixExpr(exprType, op, left, right, isLeftAssoc) =>
        gen(left)
        str(" ")
        gen(op)
        str(" ")
        gen(right)
      case ParenthesesExpr(inner) =>
        str("(")
        gen(inner)
        str(")")
      case LambdaExpr(exprType, params, expr, needBraces) =>
        str("{ ")
        if (needBraces) str("(")
        rep(params, ", ") { case DefParameter(exprType, name) =>
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

      case TypeExpr(exprType) =>
        genType(exprType, false)

      case CallExpr(exprType, ref, params) =>
        gen(ref)
        if (params.size == 1 && params.head.isInstanceOf[LambdaExpr]) {
          str(" ")
          gen(params.head)
        } else {
          str("(")
          rep(params, ", ")(gen)
          str(")")
        }

      case RefExpr(exprType, obj, ref, typeParams, isFunc) =>
        opt(obj) { x => gen(x); str(".") }
        str(ref)
        if (typeParams.nonEmpty) {
          str("<")
          rep(typeParams, ", ")(gen)
          str(">")
        }

      case IfExpr(exprType, cond, trueB, falseB) =>
        str("if (")
        gen(cond)
        str(")")
        gen(trueB)
        opt(falseB) { b =>
          str(" else ")
          gen(b)
        }
      case PostfixExpr(exprType, obj, op) =>
        gen(obj)
        str(op)

      case LitExpr(exprType, name) =>
        str(name)
      case UnderscoreExpr(exprType) =>
        str("it")

      case WhenExpr(exprType, expr, clauses) =>
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
      case NewExpr(exprType, name, args) =>
        str(name)
        str("(")
        rep(args, ", ")(gen)
        str(")")

      case e: BlockExpr =>
        genAsBlock(e)

      case ForInExpr(exprType, ref, range, body) =>
        str("for (")
        gen(ref)
        str(" in ")
        gen(range)
        str(") ")
        gen(body)

      case InterpolatedStringExpr(parts, injected) =>
        scoped(
          stateVal.updated(_.copy(inInterpolatedString = true))
        ) {
          str("\"")
          rep(parts.zip(injected), "") { case (p, i) =>
            str(p)
            str("$")
            gen(i)
          }
          str(parts.last)
          str("\"")
        }

      case BracketsExpr(exprType, expr, inBrackets) =>
        gen(expr)
        str("[")
        gen(inBrackets)
        str("]")
      case ThisExpr(exprType) =>
        str("this")

      case TypeParam(exprType) =>
        genType(exprType, false)

      case x: Keyword =>
        genKeyword(x)
    }

  def genAsBlock(e: Expr): Unit = e match {
    case BlockExpr(exprType, exprs) =>
      str("{")
      if (!stateVal.inInterpolatedString) indent()
      repNl(exprs)(gen)
      if (!stateVal.inInterpolatedString) unIndent()
      str("}")
    case _ =>
      genAsBlock(BlockExpr(NoType, Seq(e)))
  }

  def genKeyword(k: Keyword): Unit =
    str(k.name)

  def genType(t: Type, pref: Boolean = true): Unit = {
    if (pref) str(": ")
    str(t.asKotlin)
  }
}
