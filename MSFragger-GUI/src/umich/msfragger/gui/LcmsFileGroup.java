package umich.msfragger.gui;

import java.util.List;

public class LcmsFileGroup {
    public final String groupName;
    public final List<InputLcmsFile> inputLcmsFiles;

    public LcmsFileGroup(String groupName, List<InputLcmsFile> inputLcmsFiles) {
        this.groupName = groupName;
        this.inputLcmsFiles = inputLcmsFiles;
    }
}
