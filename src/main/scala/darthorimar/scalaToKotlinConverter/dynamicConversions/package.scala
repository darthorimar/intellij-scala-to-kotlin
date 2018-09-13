package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast.Expr

package object dynamicConversions {

  case class ConversionSource(parameters: String, scala: String, kotlin: String)

  case class Conversion(parameters: Seq[Expr],
                        scalaTemplate: Expr,
                        kotlinTemplate: String)

}
