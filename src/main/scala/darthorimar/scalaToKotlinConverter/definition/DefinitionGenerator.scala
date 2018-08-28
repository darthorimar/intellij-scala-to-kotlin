package darthorimar.scalaToKotlinConverter.definition

import com.intellij.psi.{PsiDirectory, PsiDocumentManager, PsiFile}
import darthorimar.scalaToKotlinConverter.Utils
import org.jetbrains.kotlin.psi.{KtFile, KtFunction, KtNamedFunction}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object DefinitionGenerator {
  final val packageName = "convertedFromScala.lib"
  final val libFileName = "lib.kt"

  private def collectDefinitions(definitions: Seq[Definition]): Seq[Definition] = {
    val visited = mutable.Map.empty[String, Definition]
    var stack: List[Definition] = definitions.toList
    while (stack.nonEmpty) {
      val definition = {
        val head = stack.head
        stack = stack.tail
        head
      }
      if (!visited.contains(definition.name)) {
        visited += definition.name -> definition
        for (dependency <- definition.dependencies) {
          stack = dependency :: stack
        }
      }
    }
    visited.values.toSeq
  }

  private def generateDefinitionsText(definitions: Seq[Definition]): String =
    definitions map {
      case definition: FileDefinition =>
        val filename = definition.filename
        Source
          .fromInputStream(getClass
            .getResourceAsStream(s"/darthorimar/scalaToKotlinConverter/stdlib/$filename"))
          .getLines()
          .mkString("\n")
      case textDefinition: TextDefinition =>
        textDefinition.get
    } mkString "\n\n"

  private def getOrCreateLibFile(baseDirectory: PsiDirectory): KtFile = {
    val libDirectory =
      Try {
        (baseDirectory /: packageName.split('.')) (_.findSubdirectory(_))
      } orElse
        Try {
          (baseDirectory /: packageName.split('.')) (_.createSubdirectory(_))
        } getOrElse baseDirectory

    Option(libDirectory.findFile(libFileName))
      .getOrElse {
        val file = libDirectory.createFile(libFileName)
        val document = PsiDocumentManager.getInstance(file.getProject).getDocument(file)
        document.insertString(0, s"package $packageName \n\n")
        PsiDocumentManager.getInstance(file.getProject).commitDocument(document)
        file
      }.asInstanceOf[KtFile]
  }

  def generate(definitions: Seq[Definition], baseDirectory: PsiDirectory): Unit = {
    if (definitions.nonEmpty) {
      val file = getOrCreateLibFile(baseDirectory)
      val collectedDefinitions = collectDefinitions(definitions)
      val existingDefinitionNames = getExistingDefinitionNames(file)
      val definitionsToGenerate = collectedDefinitions.filter { definition =>
        !existingDefinitionNames.contains(definition.name)
      }

      val text = generateDefinitionsText(definitionsToGenerate)

      val document = PsiDocumentManager.getInstance(file.getProject).getDocument(file)
      document.insertString(document.getTextLength, text)
      PsiDocumentManager.getInstance(file.getProject).commitDocument(document)
      Utils.reformatKtElement(file)
    }
  }

  def getExistingDefinitionNames(file: PsiFile): Seq[String] = file match {
    case ktFile: KtFile =>
      ktFile.getDeclarations.asScala map {
        case funct: KtNamedFunction if funct.getReceiverTypeReference != null =>
          val typeName = funct.getReceiverTypeReference.getText.takeWhile(_.isLetter).toLowerCase
          val funName = funct.getName.toLowerCase.capitalize
          s"$typeName$funName"
        case decl => decl.getName
      }
    case _ => Seq.empty
  }
}