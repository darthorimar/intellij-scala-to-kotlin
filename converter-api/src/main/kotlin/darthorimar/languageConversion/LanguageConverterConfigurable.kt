package darthorimar.languageConversion

import com.intellij.openapi.options.SearchableConfigurable
import javax.swing.JComponent

class LanguageConverterConfigurable : SearchableConfigurable {
    companion object {
        const val ID = "language.converters"
    }

    override fun isModified(): Boolean = false

    override fun getId(): String = ID

    override fun getDisplayName(): String = "Language Converters"

    override fun apply() {}

    override fun createComponent(): JComponent? = null
}