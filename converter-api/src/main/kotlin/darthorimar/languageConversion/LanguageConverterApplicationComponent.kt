package darthorimar.languageConversion

import com.intellij.openapi.actionSystem.ActionManager
import com.intellij.openapi.components.ApplicationComponent
import darthorimar.languageConversion.ConvertFileAction.Companion.ACTION_PREFIX

class LanguageConverterApplicationComponent : ApplicationComponent {
    override fun initComponent() {
        val actionManager = ActionManager.getInstance()

        val converterExtentionPoints = LanguageConverterExtension.EP_NAME.extensions
        for (converter in converterExtentionPoints) {
           val converterAction = actionManager.registerAction(ACTION_PREFIX + converter.name, ConvertFileAction())
        }

    }

    override fun getComponentName(): String = "LanguageConverterApplicationComponent"
}