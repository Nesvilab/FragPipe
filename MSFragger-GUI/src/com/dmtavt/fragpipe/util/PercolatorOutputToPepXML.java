package com.dmtavt.fragpipe.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PercolatorOutputToPepXML {
    public static void main(final String[] args) {
        if (args.length == 0)
            percolatorToPepXML(
                    Paths.get("/home/ci/percolator_test/23aug2017_hela_serum_timecourse_4mz_narrow_1.pin"),
                    Paths.get("/home/ci/percolator_test/23aug2017_hela_serum_timecourse_4mz_narrow_1.pepXML"),
                    Paths.get("/home/ci/percolator_test/percolator_results_psms.tsv"),
                    Paths.get("/home/ci/percolator_test/percolator_decoy_results_psms.tsv"),
                    Paths.get("/home/ci/percolator_test/test.pep.xml"));
        else
            percolatorToPepXML(
                    Paths.get(args[0]),
                    Paths.get(args[1]),
                    Paths.get(args[2]),
                    Paths.get(args[3]),
                    Paths.get(args[4])
            );
    }

    static String getSpectrum(final String line) {
        String spectrum = null;
        for (final String e : line.split("\\s"))
            if (e.startsWith("spectrum=")) {
                spectrum = e.substring("spectrum=\"".length(), e.length() - 1);
                break;
            }
        return spectrum.substring(0, spectrum.length() - 2);
    }
    static double getMassdiffPPM(final String line) {
        double massdiff = Double.NaN;
        double calc_neutral_pep_mass = Double.NaN;
        for (final String e : line.split("\\s")) {
            if (e.startsWith("massdiff=")) {
                massdiff = Double.parseDouble(e.substring("massdiff=\"".length(), e.length() - 1));
            }
            if (e.startsWith("calc_neutral_pep_mass=")) {
                calc_neutral_pep_mass = Double.parseDouble(e.substring("calc_neutral_pep_mass=\"".length(), e.length() - 1));
                break;
            }
        }
        return massdiff * 1e6 / calc_neutral_pep_mass;
    }

    public static void percolatorToPepXML(final Path pin,
            final Path pepxml, final Path percolatorTargetPsms, final Path percolatorDecoyPsms, final Path output) {
        final Map<String, double[]> percolator_dict = new HashMap<>();
        for (final Path tsv : new Path[]{percolatorTargetPsms, percolatorDecoyPsms})
            try (final BufferedReader brtsv = Files.newBufferedReader(tsv)) {
                final String percolator_header = brtsv.readLine();
                final List<String> colnames = Arrays.asList(percolator_header.split("\t"));
                final int indexOfPSMId = colnames.indexOf("PSMId");
                final int indexOfPEP = colnames.indexOf("posterior_error_prob");
                final int indexOfScore = colnames.indexOf("score");
                String line;
                while ((line = brtsv.readLine()) != null) {
                    final String[] split = line.split("\t");
                    final String raw_psmid = split[indexOfPSMId];
                    final String psmid = raw_psmid.substring(0, raw_psmid.lastIndexOf("."));
                    final double pep = Double.parseDouble(split[indexOfPEP]);
                    final double score = Double.parseDouble(split[indexOfScore]);
                    if (percolator_dict.containsKey(psmid))
                        throw new AssertionError();
                    percolator_dict.put(psmid, new double[]{pep, score});
                }
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        final Map<String, Object[]> pin_dict = new HashMap<>();

        try (final BufferedReader brtsv = Files.newBufferedReader(pin)) {
            final String pin_header = brtsv.readLine();
            final List<String> colnames = Arrays.asList(pin_header.split("\t"));
            final int indexOf_SpecId = colnames.indexOf("SpecId");
            final int indexOf_ntt = colnames.indexOf("ntt");
            final int indexOf_nmc = colnames.indexOf("nmc");
            final int indexOf_abs_mass_diff = colnames.indexOf("abs_mass_diff");
            String line;
            while ((line = brtsv.readLine()) != null) {
                final String[] split = line.split("\t");
                final String raw_SpecId = split[indexOf_SpecId];
                final String SpecId = raw_SpecId.substring(0, raw_SpecId.lastIndexOf("."));
                final int ntt = Integer.parseInt(split[indexOf_ntt]);
                final int nmc = Integer.parseInt(split[indexOf_nmc]);
                final double abs_mass_diff = Double.parseDouble(split[indexOf_abs_mass_diff]);
                if (pin_dict.containsKey(SpecId))
                    throw new AssertionError();
                pin_dict.put(SpecId, new Object[]{ntt, nmc, abs_mass_diff});
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        try (final BufferedReader brpepxml = Files.newBufferedReader(pepxml);
             final BufferedWriter out = Files.newBufferedWriter(output)) {
            String line;
            while ((line = brpepxml.readLine()) != null) {
                out.write(line + "\n");
                if (line.trim().equals("</search_summary>"))
                    break;
            }
            String spectrum;
            double massdiffPPM = Double.NaN;
            long num_psms = 0;
            final StringBuilder sb = new StringBuilder();
            while ((line = brpepxml.readLine()) != null) {
                if (line.trim().startsWith("<spectrum_query")) {
                    sb.setLength(0);
                    spectrum = getSpectrum(line);
                    final double[] pep_score = percolator_dict.get(spectrum);
                    final double one_minus_PEP = 1 - pep_score[0];
                    final double score = pep_score[1];
                    final Object[] ntt_nmc_absmassdiff = pin_dict.get(spectrum);
                    final int ntt = (int) ntt_nmc_absmassdiff[0];
                    final int nmc = (int) ntt_nmc_absmassdiff[1];
                    ++num_psms;
                    sb.append(line).append('\n');
                    while ((line = brpepxml.readLine()) != null)
                        if (line.trim().startsWith("<search_hit ")) {
                            massdiffPPM = getMassdiffPPM(line);
                            break;
                        }
                    while ((line = brpepxml.readLine()) != null) {
                        if (line.trim().equals("</search_hit>")) {
                            sb.append(
                                    String.format(
                                            "<analysis_result analysis=\"peptideprophet\">\n" +
                                                    "<peptideprophet_result probability=\"%f\" all_ntt_prob=\"(%f,%f,%f)\">\n" +
                                                    "<search_score_summary>\n" +
                                                    "<parameter name=\"fval\" value=\"%f\"/>\n" +
                                                    "<parameter name=\"ntt\" value=\"%d\"/>\n" +
                                                    "<parameter name=\"nmc\" value=\"%d\"/>\n" +
                                                    "<parameter name=\"massd\" value=\"%f\"/>\n" +
                                                    "<parameter name=\"isomassd\" value=\"%d\"/>\n" +
                                                    "</search_score_summary>" +
                                                    "</peptideprophet_result>\n" +
                                                    "</analysis_result>\n",
                                            one_minus_PEP, 0.3333, 0.3333, 0.3333,
                                            score, ntt, nmc, massdiffPPM, -1
                                    ));
                        }
                        sb.append(line).append("\n");
                        if (line.trim().equals("</spectrum_query>")) {
                            out.write(sb.toString());
                            break;
                        }
                    }
                }
            }
            System.out.println("num_psms = " + num_psms);
            System.out.println("percolator_dict.size() = " + percolator_dict.size());
            System.out.println("pin_dict.size() = " + pin_dict.size());
            out.write("</msms_run_summary>\n" +
                    "</msms_pipeline_analysis>");
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
