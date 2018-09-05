package darthorimar.scalaToKotlinConverter.step
open class GenerateKtElementStep() : ConverterStep<String, KtElement> {
  override fun apply(from: String, state: ConverterStepState): Pair<KtElement, ConverterStepState> {
     val ktElement: KtElement = state.elementGenerator!!.insertCode(from)
     val file: PsiFile = ktElement.getContainingFile()
    generateDefinitions(state.collectedDefinitions(), (file as KtFile))
    generateImports(state.collectImports(), (file as KtFile))
    PsiDocumentManager.getInstance(ktElement.getProject()).commitDocument(PsiDocumentManager.getInstance(ktElement.getProject()).getDocument(ktElement.getContainingFile()))
     val formated: KtElement = Utils.reformatKtElement(ktElement)
    return Pair<KtElement, ConverterStepState>(formated, state)
  }
  private fun generateDefinitions(definitions: List<Definition>, ktFile: KtFile): Unit {
    DefinitionGenerator.generate(definitions, ktFile.getContainingDirectory())
  }
  private fun generateImports(imports: List<Import>, ktFile: KtFile): Unit {
    ApplicationManager.getApplication().invokeAndWait { inWriteAction { imports.sortBy(({ it.ref })).forEach(({ generateSingleImport(it, ktFile) })) } }
  }
  private fun generateSingleImport(imp: Import, ktFile: KtFile): Unit {
     val match = imp
    data class `Import(ref)_data`(public val ref: String)
    val `Import(ref)` by lazy {
      if (match is Import) {
         val (ref) = match
        if (ref is String) {
          return@lazy `Import(ref)_data`(ref)
        }
      }
      return@lazy null
    }
    when {
      `Import(ref)` != null -> {
         val (ref) = `Import(ref)`
         val fqName: FqName = FqName(ref)
        J2kPostProcessor(false).insertImport(ktFile, fqName)
      }
      else -> throw MatchError(match)
    }
  }
}