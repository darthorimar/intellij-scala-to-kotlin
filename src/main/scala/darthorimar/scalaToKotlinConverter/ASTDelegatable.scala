package darthorimar.scalaToKotlinConverter

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object ASTDelegatable {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      println(annottees.map(_.tree).toList)
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" :: Nil =>
          val newParamss = List(paramss.head collect {
            case q"$mods val $tname: $tpt = $expr" =>
              q"private var ${TermName("_" + tname.toString)}: $tpt = $expr"
          })

          val getters = paramss.head collect {
            case q"$mods val $tname: $tpt = $expr" =>
              q"""def $tname = ${TermName("_" + tname.toString)}"""
          }
          val setters = paramss.head collect {
            case q"$mods val $tname: $tpt = $expr" =>
              q""" def ${TermName(tname.toString + "_$eq")}(v: $tpt): Unit = {
                     println("HI")
                     ${TermName("_" + tname.toString)} = v
               }"""
          }

          val result =
            q"""$mods class $tpname[..$tparams] $ctorMods(...$newParamss) extends { ..$earlydefns } with ..$parents { $self =>
               ..$getters
               ..$setters
               ..$stats
             }"""
          println(result)
          result

        case _ =>
          c.abort(
            c.enclosingPosition,
            "Anotattion @delegatable can be nly used on classes"
          )
      }
    }
    c.Expr[Any](result)
  }
  @compileTimeOnly("enable macro paradise to expand macro annotations")
  final class delegatable extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro impl

  }

}
