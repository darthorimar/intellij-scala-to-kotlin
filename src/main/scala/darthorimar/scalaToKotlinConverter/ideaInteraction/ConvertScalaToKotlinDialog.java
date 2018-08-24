package darthorimar.scalaToKotlinConverter.ideaInteraction;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ConvertScalaToKotlinDialog extends DialogWrapper {
    private JPanel panel;
    private JButton buttonOK;

    public ConvertScalaToKotlinDialog(Project project) {
        super(project, true);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);
        setTitle("Convert Scala to Kotlin Code");
        init();
    }

    @Nullable
    @Override
    protected JComponent createCenterPanel() {
        return panel;
    }
}
