/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JTabbedPane;
import org.nesvilab.fragpipe.api.FragpipeCacheUtils;
import org.nesvilab.fragpipe.api.PropsFile;
import org.nesvilab.fragpipe.tabs.TabWorkflow;
import org.nesvilab.utils.PropertiesUtils;
import org.nesvilab.utils.StringUtils;

/**
 * Utility to programmatically load all built-in workflows and re-save them to a custom folder.
 * This is useful for "refreshing" workflows when new UI parameters have been added,
 * as loading a workflow into the UI will apply default values for any new parameters.
 *
 * This utility is designed to be called from within the FragPipe GUI, not as a standalone program.
 * See exportAllWorkflowsFromGui() method.
 */
public class WorkflowExporter {

  private static final String WORKFLOW_EXT = ".workflow";

  /**
   * Export all built-in workflows to the specified output directory.
   * This method should be called from within the running FragPipe GUI.
   *
   * @param outputDir Directory where workflows will be saved
   * @param tabs The JTabbedPane containing all FragPipe tabs
   * @return Number of workflows exported
   */
  @SuppressWarnings("unchecked")
  public static int exportAllWorkflowsFromGui(Path outputDir, JTabbedPane tabs) {
    Path workflowsDir = FragpipeLocations.get().getDirWorkflows();

    try {
      Files.createDirectories(outputDir);
    } catch (IOException e) {
      System.err.println("Failed to create output directory: " + outputDir);
      return 0;
    }

    List<Path> workflowFiles;
    try (Stream<Path> walk = Files.walk(workflowsDir)) {
      workflowFiles = walk
          .filter(Files::isRegularFile)
          .filter(p -> p.toString().endsWith(WORKFLOW_EXT))
          .collect(Collectors.toList());
    } catch (IOException e) {
      System.err.println("Failed to list workflow files: " + e.getMessage());
      return 0;
    }

    System.out.println("Found " + workflowFiles.size() + " workflow files in: " + workflowsDir);

    int count = 0;
    for (Path workflowFile : workflowFiles) {
      String workflowName = StringUtils.upToLastDot(workflowFile.getFileName().toString());

      try {
        // Load the workflow file
        PropsFile propsFile = new PropsFile(workflowFile, "Workflow: " + workflowName);
        propsFile.load();

        // Convert to Map for tabsLoad
        Map<String, String> propsMap = (Map<String, String>) (Map<?, ?>) PropertiesUtils.toMap(propsFile);

        // Load into UI (this applies defaults for new parameters)
        FragpipeCacheUtils.tabsLoad(propsMap, tabs);

        // Save the workflow from current UI state
        Properties uiProps = FragpipeCacheUtils.tabsSave0(tabs, false);

        // Remove user-specific paths that should not be in built-in workflows
        uiProps.remove("database.db-path");

        String desc = propsFile.getProperty(TabWorkflow.PROP_WORKFLOW_DESC, "");

        Path outputPath = outputDir.resolve(workflowName + WORKFLOW_EXT);
        TabWorkflow.saveWorkflow(outputPath, desc, uiProps);

        count++;
        System.out.println("  Exported: " + workflowName);

      } catch (Exception e) {
        System.err.println("  Failed to process " + workflowName + ": " + e.getMessage());
      }
    }

    System.out.println("Exported " + count + " of " + workflowFiles.size() + " workflows to: " + outputDir);
    return count;
  }

  /**
   * Export all built-in workflows using the current FragPipe instance.
   * Call this from anywhere in the running application.
   *
   * @param outputDir Directory where workflows will be saved
   * @return Number of workflows exported
   */
  public static int exportAllWorkflows(Path outputDir) {
    Fragpipe fp = Fragpipe.getStickyStrict(Fragpipe.class);
    return exportAllWorkflowsFromGui(outputDir, fp.tabs);
  }
}
