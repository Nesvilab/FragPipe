package com.dmtavt.fragpipe.tools.percolator;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PercolatorOutputToPepXML {

    private static final Pattern pattern = Pattern.compile("(.+spectrum=\".+\\.)([0-9]+)\\.([0-9]+)(\\.[0-9]+\".+)");

    public static void main(final String[] args) {
        Locale.setDefault(Locale.US);
        if (args.length == 0)
//            percolatorToPepXML(
//                    Paths.get("/home/ci/percolator_test/23aug2017_hela_serum_timecourse_4mz_narrow_1.pin"),
//                    "/home/ci/percolator_test/23aug2017_hela_serum_timecourse_4mz_narrow_1",
//                    Paths.get("/home/ci/percolator_test/percolator_results_psms.tsv"),
//                    Paths.get("/home/ci/percolator_test/percolator_decoy_results_psms.tsv"),
//                    Paths.get("/home/ci/percolator_test/test"),
//                    "DIA");
            percolatorToPepXML(
                    Paths.get("/home/ci/percolator_test/ranked/23aug2017_hela_serum_timecourse_4mz_narrow_1.pin"),
                    "/home/ci/percolator_test/ranked/23aug2017_hela_serum_timecourse_4mz_narrow_1",
//                    "/home/ci/percolator_test/23aug2017_hela_serum_timecourse_4mz_narrow_1",
                    Paths.get("/home/ci/percolator_test/percolator_results_psms.tsv"),
                    Paths.get("/home/ci/percolator_test/percolator_decoy_results_psms.tsv"),
                    Paths.get("/home/ci/percolator_test/test2"),
                    "DIA");
        else
            percolatorToPepXML(
                    Paths.get(args[0]),
                    args[1],
                    Paths.get(args[2]),
                    Paths.get(args[3]),
                    Paths.get(args[4]),
                    args[5]
            );
    }

    private static String getSpectrum(final String line) {
        String spectrum = null;
        for (final String e : line.split("\\s"))
            if (e.startsWith("spectrum=")) {
                spectrum = e.substring("spectrum=\"".length(), e.length() - 1);
                break;
            }
        return spectrum.substring(0, spectrum.lastIndexOf("."));
    }

    private static String paddingZeros(final String line) {
        Matcher matcher = pattern.matcher(line);
        if (matcher.matches()) {
            if (matcher.group(2).contentEquals(matcher.group(3))) {
                String scanNum = matcher.group(2);
                if (scanNum.length() < 5) {
                    StringBuilder sb = new StringBuilder(5);
                    for (int i = 0; i < 5 - scanNum.length(); ++i) {
                        sb.append("0");
                    }
                    sb.append(scanNum);
                    return matcher.group(1) + sb + "." + sb + matcher.group(4);
                } else {
                    return line;
                }
            } else {
                throw new RuntimeException("Cannot parse spectrum ID from  " + line);
            }
        } else {
            throw new RuntimeException("Cannot parse line " + line);
        }
    }

    private static class Spectrum_rank {
        final String spectrum;
        final int rank;

        Spectrum_rank(String spectrum, int rank) {
            this.spectrum = spectrum;
            this.rank = rank;
        }
    }

    private static Spectrum_rank get_spectrum_rank(final String s){
        final String charge_rank = s.substring(s.lastIndexOf("."));
        final int rank = Integer.parseInt(charge_rank.split("_")[1]);
        return new Spectrum_rank(s.substring(0, s.lastIndexOf(".")), rank);
    }

    private static int get_max_rank(final String basename, final boolean is_DIA) {
        final Path pathDIA = Paths.get(basename + "_rank1.pepXML");
        final Path pathDDA = Paths.get(basename + ".pepXML");
        final Path path = is_DIA ? pathDIA : pathDDA;

        final Pattern compile = Pattern.compile("<parameter name=\"output_report_topN\" value=\"(\\d+)\"/>");
        try (final BufferedReader br = Files.newBufferedReader(path)) {
            String line;
            while ((line = br.readLine()) != null) {
                final Matcher matcher = compile.matcher(line.trim());
                if (matcher.find())
                    return Integer.parseInt(matcher.group(1));
            }
        } catch (IOException e) {
            System.err.println("Cannot find output_report_topN parameter from " + path.toAbsolutePath());
            System.exit(1);
            return -1;
        }
        System.err.println("Cannot find output_report_topN parameter from " + path.toAbsolutePath());
        System.exit(1);
        return -1;
    }

    private static StringBuilder handle_search_hit(final List<String> sh, final Object[] tmp, final int search_rank, final int output_rank) {
        final StringBuilder sb = new StringBuilder();
        double calc_neutral_pep_mass = Double.NaN;
        double massdiff = Double.NaN;
        int isomassd = 0;
        final Iterator<String> iterator = sh.iterator();
        final String search_hit_line = iterator.next();
        for (final String e : search_hit_line.split("\\s")) { // fixme: the code assumes that all attributes are in one line, which makes it not robust
            if (e.startsWith("massdiff="))
                massdiff = Double.parseDouble(e.substring("massdiff=\"".length(), e.length() - 1));
            if (e.startsWith("calc_neutral_pep_mass="))
                calc_neutral_pep_mass = Double.parseDouble(e.substring("calc_neutral_pep_mass=\"".length(), e.length() - 1));
        }
        double gap = Double.MAX_VALUE;
        for (int isotope = -6; isotope < 7; ++isotope) {
            if (Math.abs(massdiff - isotope * 1.0033548378) < gap) {
                gap = Math.abs(massdiff - isotope * 1.0033548378);
                isomassd = isotope;
            }
        }
        if (gap > 0.1) { // It may be from an open search.
            isomassd = 0;
        }
        sb.append(search_rank == output_rank ? search_hit_line :
                        search_hit_line.replace("hit_rank=\"" + search_rank + "\"", "hit_rank=\"" + output_rank + "\""))
                .append("\n");
        String line;
        while (!(line = iterator.next()).trim().contentEquals("</search_hit>"))
            sb.append(line).append("\n");
        {
            final double[] pep_score = (double[]) tmp[1];
            if (pep_score == null)
                throw new IllegalStateException();
            final double one_minus_PEP = 1 - pep_score[0];
            final double score = pep_score[1];
            final int[] ntt_nmc = (int[]) tmp[0];
            final int ntt = ntt_nmc[0];
            final int nmc = ntt_nmc[1];
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
                                    "</search_score_summary>\n" +
                                    "</peptideprophet_result>\n" +
                                    "</analysis_result>\n",
                            one_minus_PEP, one_minus_PEP, one_minus_PEP, one_minus_PEP,
                            score, ntt, nmc, (massdiff - isomassd * 1.0033548378) * 1e6 / calc_neutral_pep_mass, isomassd
                    ));
        }
        sb.append("</search_hit>\n");
        return sb;
    }

    private static String handle_spectrum_query(final List<String> sq,
                                                final Map<String, Object[][]> pin_tsv_dict_r,
                                                final boolean is_DIA, final int DIA_rank) {
        final List<List<String>> search_hits = new ArrayList<>();
        final StringBuilder sb = new StringBuilder();
        String spectrum;
        final Iterator<String> iterator = sq.iterator();
        for (String line; iterator.hasNext(); ) {
            line = iterator.next().trim();
            spectrum = getSpectrum(line);
            final Object[][] tmp = pin_tsv_dict_r.get(spectrum);
            sb.append(paddingZeros(line)).append('\n');
            while (iterator.hasNext()) { // fixme: the code assumes that there are always <search_hit, massdiff=, and calc_neutral_pep_mass=, which makes it not robust
                line = iterator.next().trim();
                if (line.startsWith("<search_result>"))
                    sb.append(line).append('\n');
                else if (line.trim().startsWith("<search_hit ")) {
                    final ArrayList<String> search_hit = new ArrayList<>();
                    search_hit.add(line);
                    do {
                        line = iterator.next();
                        search_hit.add(line);
                    } while (!line.contentEquals("</search_hit>"));
                    search_hits.add(search_hit);
                } else if (line.trim().startsWith("</search_result>")) {
                    if (is_DIA)
                        sb.append(handle_search_hit(search_hits.get(0), tmp[DIA_rank - 1], 1, 1));
                    else {
                        // write the search_hits ordered by Percolator
                        final TreeMap<Double, Integer> tm = new TreeMap<>(Collections.reverseOrder());
                        for (int ranki = 0; ranki < tmp.length; ++ranki) {
                            final Object[] tmp1 = tmp[ranki];
                            if (tmp1 == null)
                                continue;
                            final double[] pep_score = (double[]) tmp1[1];
                            if (pep_score == null)
                                continue;
                            tm.put(pep_score[1], ranki);
                        }
                        int output_rank = 1;
                        for (final Map.Entry<Double, Integer> entry : tm.entrySet()) {
                            final int ranki = entry.getValue();
                            sb.append(handle_search_hit(search_hits.get(ranki), tmp[ranki], ranki + 1, output_rank++));
                        }
                    }
                    sb.append(line).append('\n');
                } else if (line.trim().startsWith("</spectrum_query>"))
                    sb.append(line).append('\n');
                else
                    throw new IllegalStateException(line);
            }
        }
        return sb.toString();
    }

    public static void percolatorToPepXML(final Path pin,
                                          final String basename,
                                          final Path percolatorTargetPsms, final Path percolatorDecoyPsms,
                                          final Path outBasename,
                                          final String DIA_DDA) {

        // get max rank from pin
        final boolean is_DIA = DIA_DDA.equals("DIA");
        final int max_rank = get_max_rank(basename, is_DIA);
        if (max_rank < 1) {
            System.err.println("Cannot find output_report_topN parameter from " + basename + "'s pepXML file.");
            System.exit(1);
        }
        final Map<String, Object[][]> pin_tsv_dict_r = new HashMap<>();

        try (final BufferedReader brtsv = Files.newBufferedReader(pin)) {
            final String pin_header = brtsv.readLine();
            final List<String> colnames = Arrays.asList(pin_header.split("\t"));
            final int indexOf_SpecId = colnames.indexOf("SpecId");
            final int indexOf_ntt = colnames.indexOf("ntt");
            final int indexOf_nmc = colnames.indexOf("nmc");
            String line;
            while ((line = brtsv.readLine()) != null) {
                final String[] split = line.split("\t");
                final String raw_SpecId = split[indexOf_SpecId];
                final Spectrum_rank spectrum_rank = get_spectrum_rank(raw_SpecId);
                final String SpecId = spectrum_rank.spectrum;
                final int rank = spectrum_rank.rank;
                final int ntt = Integer.parseInt(split[indexOf_ntt]);
                final int nmc = Integer.parseInt(split[indexOf_nmc]);
                pin_tsv_dict_r.computeIfAbsent(SpecId, e -> new Object[max_rank][])
                        [rank - 1] = new Object[]{new int[]{ntt, nmc}, null};
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        for (final Path tsv : new Path[]{percolatorTargetPsms, percolatorDecoyPsms}) {
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
                    final Spectrum_rank spectrum_rank = get_spectrum_rank(raw_psmid);
                    final String psmid = spectrum_rank.spectrum;
                    final int rank = spectrum_rank.rank;
                    final double pep = Double.parseDouble(split[indexOfPEP]);
                    final double score = Double.parseDouble(split[indexOfScore]);
                    pin_tsv_dict_r.get(psmid)[rank - 1][1] = new double[]{pep, score};
                }
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }

        for (int rank = 1; rank <= (is_DIA ? max_rank : 1); ++rank) {
            final Path output_rank = is_DIA ? Paths.get(outBasename + "_rank" + rank + ".pep.xml") :
                    Paths.get(outBasename + ".pep.xml");
            final Path pepxml_rank = is_DIA ? Paths.get(basename + "_rank" + rank + ".pepXML") :
                    Paths.get(basename + ".pepXML");
            // fixme: cannot parse XML line-by-line because line break is allowed everywhere, including within an attribute, in a XML. Need to parse it using JDOM or JAXB
            try (final BufferedReader brpepxml = Files.newBufferedReader(pepxml_rank);
                 final BufferedWriter out = Files.newBufferedWriter(output_rank)) {
                String line;
                while ((line = brpepxml.readLine()) != null) {
                    out.write(line + "\n");
                    if (line.trim().startsWith("<msms_pipeline_analysis ")) {
                        final String now = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(LocalDateTime.now());
                        final String tmp = String.format("<analysis_summary analysis=\"database_refresh\" time=\"%s\"/>\n" +
                                        "<analysis_summary analysis=\"interact\" time=\"%s\">\n" +
                                        "<interact_summary filename=\"%s\" directory=\"\">\n" +
                                        "<inputfile name=\"%s\"/>\n" +
                                        "</interact_summary>\n" +
                                        "</analysis_summary>\n" +
                                        "<dataset_derivation generation_no=\"0\"/>\n",
                                now, now, output_rank.toAbsolutePath(), pepxml_rank.toAbsolutePath());
                        out.write(tmp);
                    }
                    if (line.trim().equals("</search_summary>"))
                        break;
                }

                while ((line = brpepxml.readLine()) != null) {
                    if (line.trim().startsWith("<spectrum_query")) {
                        final List<String> sq = new ArrayList<>();
                        sq.add(line);
                        while ((line = brpepxml.readLine()) != null) {
                            sq.add(line);
                            if (line.trim().equals("</spectrum_query>")) {
                                out.write(handle_spectrum_query(sq, pin_tsv_dict_r, is_DIA, rank));
                                break;
                            }
                        }
                    }
                }
                out.write("</msms_run_summary>\n" +
                        "</msms_pipeline_analysis>");
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
    }
}
