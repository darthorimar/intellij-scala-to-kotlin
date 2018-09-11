package darthorimar.scalaToKotlinConverter.ideaInteraction
open class ConvertOnCopyPastPostProcessor() : CopyPastePostProcessor<ScalaToKotlinTransferableData> {
  private fun getPsiInRange(file: PsiFile, range: TextRange): PsiElement =file.depthFirst(/* ERROR converting `()`*/).filter { element -> range.contains(element.getTextRange()) }.maxBy(({ it.getTextRange().getLength() }))
  override fun collectTransferableData(file: PsiFile, editor: Editor, startOffsets: scala.Array<Int>, endOffsets: scala.Array<Int>): List<ScalaToKotlinTransferableData> =runTry { run {
     val match = file
    when {
      match is ScalaFile -> {
         val data: scala.Array<ScalaToKotlinData> = inWriteAction { (startOffsets.zip(endOffsets)).flatMap {  val match1 = it
        open class `(from, to)_data`(private val from: Int, private val to: Int) {
          companion object  {
            fun apply(from: Int, to: Int): `(from, to)_data` =`(from, to)_data`(from, to)
            fun unapply(x: `(from, to)_data`): `(from, to)_data`? =x
          }
        }
        val `(from, to)` by lazy {
          if (match1 is Tuple2) {
             val (from, to) = match1
            if (from is Int && to is Int) {
              return@lazy `(from, to)_data`(from, to)
            }
          }
          return@lazy null
        }
        when {
          `(from, to)` != null -> {
             val (from, to) = `(from, to)`
            run {
               val match2 = getPsiInRange(match, TextRange(from, to))
              when {
                match2 is ScalaPsiElement -> {
                   val ((ast, state)) = /* ERROR converting `new ScalaPsiToAstConverter(scalaFile.getProject).convert`*/(match2, ConverterStepState())
                   val data: ScalaToKotlinData = ScalaToKotlinData(from, to, ast, state)
                  listOf(data)
                }
                else -> {
                  emptyList()
                }
              }
            }
          }
          else -> throw MatchError(match1)
        } } }
        /* ERROR converting `Collections.singletonList(new ScalaToKotlinTransferableData(data))`*/
      }
      else -> {
        Collections.emptyList<ScalaToKotlinTransferableData>()
      }
    }
  } }.getOrElse { Collections.emptyList<ScalaToKotlinTransferableData>() }
  override fun extractTransferableData(content: Transferable): List<ScalaToKotlinTransferableData> {
    return if (content.isDataFlavorSupported(ScalaToKotlinData.dataFlavor)) /* ERROR converting `Collections.singletonList(
        content.getTransferData(ScalaToKotlinData.dataFlavor).asInstanceOf[ScalaToKotlinTransferableData])`*/ else Collections.emptyList<ScalaToKotlinTransferableData>()
  }
  override fun processTransferableData(project: Project, editor: Editor, bounds: RangeMarker, caretOffset: Int, indented: Ref<Boolean>, values: List<ScalaToKotlinTransferableData>): Unit {
     val match = PsiDocumentManager.getInstance(project).getPsiFile(bounds.getDocument())
    when {
      match is KtFile -> {
        if (ConvertScalaToKotlinDialog(project).showAndGet()) {
          asScalaBufferConverter(asScalaBufferConverter(asScalaBufferConverter(asScalaBufferConverter(asScalaBufferConverter(asScalaBufferConverter(asScalaBufferConverter(asScalaBufferConverter(asScalaBufferConverter(asScalaBufferConverter(values)))))))))).asScala().foreach { transData -> transData.data.foreach { data ->  val ast: AST = data.ast
           val state: ConverterStepState = data.state
           val document: Document = editor.getDocument()
           val ktElementGenerator: KtElementGenerator = { code -> document.replaceString(bounds.getStartOffset(), bounds.getEndOffset(), code)
          PsiDocumentManager.getInstance(project).commitDocument(document)
           val generatedCodeTextRange: TextRange = TextRange(bounds.getStartOffset(), bounds.getStartOffset + code.length)
          (getPsiInRange(match, generatedCodeTextRange) as KtElement) }
          state.elementGenerator = ktElementGenerator
          /* ERROR converting `new AstToKotlinPsiConverter(project).convert`*/(ast, state) } }
        }
      }
      else -> {

      }
    }
  }
}
open class ScalaToKotlinTransferableData(public val data: scala.Array<ScalaToKotlinData>) : TextBlockTransferableData() {
  override fun getFlavor(): DataFlavor =ScalaToKotlinData.dataFlavor
  override fun getOffsetCount(): Int =data.length * 2
  override fun getOffsets(offsets: scala.Array<Int>, index: Int): Int {
     var i: Int = index
    for (ScalaToKotlinData in data) {
      offsets.apply(i) = d.startOffset
      offsets.apply(i + 1) = d.endOffset
      i.+=(2)
    }
    return i
  }
  override fun setOffsets(offsets: scala.Array<Int>, index: Int): Int {
     var i: Int = index
    for (ScalaToKotlinData in data) {
      d.startOffset = offsets.apply(i)
      d.endOffset = offsets.apply(i + 1)
      i.+=(2)
    }
    return i
  }
}
data class ScalaToKotlinData( var startOffset: Int,  var endOffset: Int,  val ast: AST,  val state: ConverterStepState) {
  companion object  {
     val dataFlavor: DataFlavor = try {
       val dataClass: Class<*> = ScalaToKotlinData.getClass()
      DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=" + dataClass.getName, "KotlinReferenceData", dataClass.getClassLoader())
    }catch (_: Throwable) {
      null
    }
  }
}