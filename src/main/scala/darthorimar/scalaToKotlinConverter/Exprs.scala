package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.types.{ KotlinTypes, StdTypes }

object Exprs {

  def isExpr(expr: Expr, ty: Type) =
    simpleInfix(StdTypes.BOOLEAN, "is", expr, TypeExpr(ty))

  def asExpr(expr: Expr, ty: Type) =
    simpleInfix(StdTypes.BOOLEAN, "as", expr, TypeExpr(ty))

  def andExpr(left: Expr, right: Expr) =
    simpleInfix(StdTypes.BOOLEAN, "&&", left, right)

  def orExpr(left: Expr, right: Expr) =
    simpleInfix(StdTypes.BOOLEAN, "||", left, right)

  def letExpr(obj: Expr, lambda: LambdaExpr) =
    CallExpr(lambda.exprType, RefExpr(NoType, Some(obj), "let", Seq.empty, true), Seq(lambda), Seq.empty)

  def simpleInfix(resultType: Type, op: String, left: Expr, right: Expr) =
    InfixExpr(
      FunctionType(right.exprType, resultType),
      RefExpr(FunctionType(right.exprType, resultType), None, op, Seq.empty, isFunctionRef = false),
      left,
      right,
      isLeftAssoc = true
    )

  def emptyList(ty: Type) =
    CallExpr(listType(ty), RefExpr(ty, None, "emptyList", Seq(ty), true), Seq.empty, Seq.empty)

  def emptyList =
    CallExpr(listType(NoType), RefExpr(NoType, None, "emptyList", Seq.empty, true), Seq.empty, Seq.empty)

  def listType(ty: Type) =
    GenericType(KotlinTypes.LIST, Seq(ty))

  def simpleCall(name: String, returnType: Type, aruments: Seq[Expr]) =
    CallExpr(FunctionType(ProductType(aruments.map(_.exprType)), returnType),
             RefExpr(returnType, None, name, Seq.empty, true),
             aruments,
             Seq.empty)

  def simpleRef(name: String, refType: Type) =
    RefExpr(refType, None, name, Seq.empty, false)

  def runExpr(expr: Expr): CallExpr =
    simpleCall("run", expr.exprType, Seq(LambdaExpr(expr.exprType, Seq.empty, expr, false)))

  def blockOrWrapped(expr: Expr): BlockExpr = expr match {
    case block: BlockExpr => block
    case _                => BlockExpr(Seq(expr))
  }

  def blockOrSingleExpr(exprs: Seq[Expr]): Expr =
    if (exprs.length == 1) exprs.head
    else BlockExpr(exprs)


  val falseLit = LitExpr(StdTypes.BOOLEAN, "false")
  val trueLit  = LitExpr(StdTypes.BOOLEAN, "true")
  val nullLit  = LitExpr(NoType, "null") //TODO fix
}
