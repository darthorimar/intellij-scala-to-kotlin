package darthorimar.languageConversion

import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.DialogWrapper

import javax.swing.*

internal class OnPasteConverterDialog(converter: LanguageConverterExtension<*, *>,
                                      project: Project) : DialogWrapper(project, true) {
    private var panel: JPanel? = null
    private var buttonOK: JButton? = null
    private var textBox: JLabel? = null

    init {
        isModal = true
        rootPane.defaultButton = buttonOK
        title = "Convert ${converter.languageFrom.displayName} to ${converter.languageTo.displayName}"
        textBox?.text = "Content copied from ${converter.languageFrom.displayName}. Do you want to convert it to ${converter.languageTo.displayName} code?"
        init()
    }

    override fun createCenterPanel(): JComponent? {
        return panel
    }
}
