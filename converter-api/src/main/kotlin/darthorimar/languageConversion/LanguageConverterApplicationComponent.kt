package darthorimar.languageConversion

import com.intellij.openapi.actionSystem.ActionManager
import com.intellij.openapi.actionSystem.DefaultActionGroup
import com.intellij.openapi.components.ApplicationComponent
import com.intellij.openapi.extensions.ExtensionPoint
import com.intellij.openapi.extensions.Extensions
import org.jdom.Element

internal class LanguageConverterApplicationComponent : ApplicationComponent {
    override fun initComponent() {
        val converters = LanguageConverterExtension.EP_NAME.extensions
        for (converter in converters) {
            registerConvertAction(converter)
        }
    }


    private fun registerConvertAction(converter: LanguageConverterExtension<*, *>) {
        val actionManager = ActionManager.getInstance()
        val converterAction = ConvertFileAction(converter)
        val actionId = "${ConvertFileAction.ACTION_PREFIX}." +
                "${converter.languageFrom.displayName.capitalize()}To${converter.languageTo.displayName.capitalize()}"
        actionManager.registerAction(actionId, converterAction)
        fun addToGroup(groupName: String) {
            val refactorActionGroup = actionManager.getAction(groupName) as DefaultActionGroup
            refactorActionGroup.add(converterAction)
        }
        for (actionGroupName in actionGroupNames) {
            addToGroup(actionGroupName)
        }
    }

    override fun getComponentName(): String = "LanguageConverterApplicationComponent"

    companion object {
        private val actionGroupNames: Set<String>
            get() = setOf("RefactoringMenu", "EditorTabPopupMenu", "ProjectViewPopupMenu")
    }
}