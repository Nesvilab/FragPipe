package com.dmtavt.fragpipe.tools.downstream;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.net.URL;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.tabs.TabRun;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.SwingUtils;
import javafx.application.Application;
import javafx.concurrent.Task;
import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextInputControl;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

// https://reprint-apms.org/?q=tutorial
// https://reprint-apms.org/?q=tutorial2_0

public class DownstreamPanel extends Application{

    @FXML
    private ResourceBundle resources;

    @FXML
    private Spinner<Integer> saintexpress_MaxRep;

    @FXML
    private Spinner<Integer> saintexpress_virtCtrls;

    @FXML
    private Button run_saintexpress;

    @FXML
    private TextArea output;
    @FXML
    private Button clear_output;
    @FXML
    private Hyperlink cite;
    final Executor exec = Executors.newFixedThreadPool(2, runnable -> {
        Thread t = new Thread(runnable);
        t.setDaemon(true);
        return t;
    });
    @FXML
    void initialize() {
        saintexpress_MaxRep.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 100, 10));
        saintexpress_virtCtrls.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 10000, 100));
        clear_output.setOnAction(event -> output.clear());
        cite.setOnAction(event -> getHostServices().showDocument("https://doi.org/10.1016/j.jprot.2013.10.023"));

        run_saintexpress.setOnAction(event -> {
            final Task task = new Task<Void>() {
                @Override
                public Void call() {
                    final TabRun tabRun = Bus.getStickyEvent(TabRun.class);
                    final String wdStr = tabRun.getWorkdirText();
                    final Path wd = Paths.get(wdStr);
                    final int R_opt = saintexpress_MaxRep.valueProperty().getValue();
                    final int L_opt = saintexpress_virtCtrls.valueProperty().getValue();
                    try {
                        execute_saintexpress(wd, R_opt, L_opt, null, output);
                    } catch (RuntimeException ex) {
                        if (ex.getCause() instanceof NoSuchFileException) {
                            output.appendText("Missing file:\n");
                            output.appendText(((NoSuchFileException) ex.getCause()).getFile());
                            output.appendText("\n");
                        } else {
                            output.appendText(org.apache.commons.lang3.exception.ExceptionUtils.getStackTrace(ex));
                        }
                    }
                    return null;
                }
            };
            exec.execute(task);
        });
    }

    @Override
    public void start(Stage primaryStage) throws Exception {

    }

    /**
     * note that we have 2 reprint files. reprint.int.tsv and .spc
     * spc always has counts, and needs to be run via spectral counting mode
     * Int file is there but may have all 0 intensities. If so, ignore it. If it has non-zero intensity, we should run SAINtexpress the second time with Intensity mode using that file.
     * Alternatively, we can add an option in FragPipe tab to run SPC, intensity (again, if non-zero), or both (but we can always run SPC, and intensity only if checked)
     * we also want # Number of Virtual Controls options
     * and also # Replicates
     * At the moment, there are two options in SAINTexpress.
     * (1) –L option: this argument sets the number of virtual control purifications by compression. For instance, if the user wishes to take 4 largest spectral counts for controls, do
     * > SAINTexpress-spc –L4 inter.dat prey.dat bait.dat
     * (2) –R option: this argument sets the number of replicates (with largest spectral counts or intensities) to be used for probability calculation in each bait. This option is useful when some baits have more replicates than others. Default is 100, using all replicates in most realistic datasets.
     * which we can change to all/best-2 option (if best-2 , pass R2) (edited)
     *
     */
    private enum Mode {
        SPECTRAL_COUNT("spc"),
        INTENSITY("int");
        private final String name;

        Mode(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return name;
        }
    }

    private static void execute_saintexpress(final Path wd, int R_opt, int L_opt, PrintStream out, TextInputControl tic) {
        for (Mode mode : Mode.values()) {
            final Path outPath = wd.resolve("saintexpress-" + mode);
            reprint_tsv_to_saint_input(wd, "reprint." + mode + ".tsv", outPath);
            final Path bin = FragpipeLocations.get().getDirTools().resolve(Map.of(Mode.SPECTRAL_COUNT, SAINT_EXPRESS_SPECTRAL_COUNTS, Mode.INTENSITY, SAINT_EXPRESS_INTENSITY).get(mode));
            try {
                final ProcessBuilder pb = new ProcessBuilder().command(List.of(bin.toString(), "-L" + L_opt, "-R" + R_opt)).redirectErrorStream(true).directory(outPath.toFile());
                final Process proc = pb.start();
                if (out == null) {
                    tic.appendText("Executing: ");
                    tic.appendText(pb.command() + "\n");
                    tic.appendText(new String(proc.getInputStream().readAllBytes()) + "\n");
                } else {
                    out.println("Executing:");
                    out.println(pb.command());
                    out.println(new String(proc.getInputStream().readAllBytes()));
                }
                proc.waitFor();
                Thread.sleep(1111);
            } catch (IOException e) {
                throw new RuntimeException(e);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
        final String cite_text = "DONE!\n" +
                "Please cite:\n" +
                "SAINTexpress: Improvements and additional features in Significance Analysis of INTeractome software\n" +
                "https://doi.org/10.1016/j.jprot.2013.10.023\n";
        if (out == null) {
            tic.appendText(cite_text);
        } else {
            out.print(cite_text);
            out.flush();
        }
    }

    public static void main(String[] args) {

        final TabRun tabRun = Bus.getStickyEvent(TabRun.class);
//    final String wdStr = tabRun.getWorkdirText();
        final String wdStr = "/run/media/ci/2b90fc78-6881-4e05-b4dd-c12b5ebc5437/1/fp_saint/fengchao";
//        final String wdStr = "/run/media/ci/2b90fc78-6881-4e05-b4dd-c12b5ebc5437/1/fp_saint/";
//    final Path wd = validateWd(tabRun, wdStr);
        final Path wd = Paths.get(wdStr);
        execute_saintexpress(wd, 10, 12, System.out, null);
    }

    public static final String SAINT_EXPRESS_SPECTRAL_COUNTS = "SAINTexpress/SAINTexpress-spc" + (OsUtils.isUnix() ? "" : ".exe");
    public static final String SAINT_EXPRESS_INTENSITY = "SAINTexpress/SAINTexpress-int" + (OsUtils.isUnix() ? "" : ".exe");

    public static void reprint_tsv_to_saint_input(final Path wd, final String input_file, final Path outPath) {
        final ArrayList<String> inter_dat = new ArrayList<>();
        final ArrayList<String> bait_dat = new ArrayList<>();
        final ArrayList<String> prey_dat = new ArrayList<>();
        try (final BufferedReader br = Files.newBufferedReader(wd.resolve(input_file))) {
            final String[] first_row = br.readLine().split("\t");
            final boolean has_protlen = first_row[2].equalsIgnoreCase("PROTLEN");
            final boolean has_geneid = first_row[1].equalsIgnoreCase("GENEID");
            final int column_offset = has_protlen ? 3 : has_geneid ? 2 : 1;
            final String[] IP = new String[first_row.length - column_offset];
            for (int i = 0; i < IP.length; i++) {
                final String s = first_row[i + column_offset];
                if (!(s.endsWith("_INT") || s.endsWith("_SPC")))
                    throw new AssertionError(s);
                IP[i] = s.substring(0, s.length() - 4);
            }
            final String[] bait_names = new String[IP.length];
            final String[] second_row = br.readLine().split("\t");
            for (int i = 0; i < IP.length; i++) {
                final String s = second_row[i + column_offset];
                bait_names[i] = s.equals("CONTROL") ? IP[i] : s;
            }
            for (int i = 0; i < IP.length; ++i)
                bait_dat.add(IP[i] + "\t" + bait_names[i] + "\t" + (bait_names[i].startsWith("CONTROL") ? "C" : "T"));
            String line;
            while ((line = br.readLine()) != null) {
                final String[] ls = line.split("\t");
                final String prey = ls[0];
                prey_dat.add(ls[0] + "\t" + (has_protlen ? ls[2] : -1) + "\t" + (has_geneid ? ls[1] : ls[0]));
                for (int i = 0; i < IP.length; ++i) {
                    final String e = ls[i + column_offset];
                    if (Double.parseDouble(e) != 0)
                        inter_dat.add(IP[i] + "\t" + bait_names[i] + "\t" + prey + "\t" + e);
                }
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        try {
            Files.createDirectory(outPath);
        } catch (FileAlreadyExistsException ignore) {
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        try (final BufferedWriter bw = Files.newBufferedWriter(outPath.resolve("inter.dat"))) {
            for (String e : inter_dat) bw.write(e + "\n");
            bw.write('\n');
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        try (final BufferedWriter bw = Files.newBufferedWriter(outPath.resolve("bait.dat"))) {
            for (String e : bait_dat) bw.write(e + "\n");
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        try (final BufferedWriter bw = Files.newBufferedWriter(outPath.resolve("prey.dat"))) {
            for (String e : prey_dat) bw.write(e + "\n");
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static DownstreamPanel controller;
    public static JFXPanel panel;

    public static JFXPanel getPanel(String componentNamePrefix) {
        final JFXPanel panel = new JFXPanel();
        DownstreamPanel.panel = panel;

        final Parent root;
        final FXMLLoader loader = new FXMLLoader(DownstreamPanel.class.getResource("DownstreamPanel.fxml"));
        try {
            root = loader.load();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        if (controller != null)
            throw new RuntimeException("this method should be called only once");
        controller = loader.getController();
//        Platform.runLater(controller::init);
        final Scene scene = new Scene(root, Color.TRANSPARENT);
        panel.setScene(scene);
        SwingUtils.jfxControlNames.put(componentNamePrefix + "saintexpress.max_replicates", controller.saintexpress_MaxRep);
        SwingUtils.jfxControlNames.put(componentNamePrefix + "saintexpress.num_virtual_controls", controller.saintexpress_virtCtrls);
        return panel;
    }

}
