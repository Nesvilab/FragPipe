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

package org.nesvilab.fragpipe.tools.percolator;

import org.nesvilab.utils.StringUtils;
import java.io.*;
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
    private static final Pattern pattern1 = Pattern.compile("base_name=\"([^\"]+)\"");
    private static final Pattern pattern2 = Pattern.compile("raw_data_type=\"([^\"]+)\"");
    private static final Pattern pattern3 = Pattern.compile("raw_data=\"([^\"]+)\"");

    public static void main(final String[] args) {
        Locale.setDefault(Locale.US);
        if (args.length == 0) {
            percolatorToPepXML(
                Paths.get("G:\\dev\\msfragger\\dev2\\5ngHeLaosmoothCE20-52lowguessSRIG450easy4_30t_C2_01_3451.pin"),
                "G:\\dev\\msfragger\\dev2\\5ngHeLaosmoothCE20-52lowguessSRIG450easy4_30t_C2_01_3451",
                Paths.get("G:\\dev\\msfragger\\dev2\\5ngHeLaosmoothCE20-52lowguessSRIG450easy4_30t_C2_01_3451_percolator_target_psms.tsv"),
                Paths.get("G:\\dev\\msfragger\\dev2\\5ngHeLaosmoothCE20-52lowguessSRIG450easy4_30t_C2_01_3451_percolator_decoy_psms.tsv"),
                Paths.get("G:\\dev\\msfragger\\dev2\\interact-5ngHeLaosmoothCE20-52lowguessSRIG450easy4_30t_C2_01_3451_2"),
                "DDA",
                0,
                "");
        } else if (Files.exists(Paths.get(args[0].replace(".pin", "_edited.pin")))){
            percolatorToPepXML(Paths.get(args[0].replace(".pin", "_edited.pin")), args[1], Paths.get(args[2]), Paths.get(args[3]), Paths.get(args[4]), args[5], Double.parseDouble(args[6]), args[7].trim());
        } else {
            percolatorToPepXML(Paths.get(args[0]), args[1], Paths.get(args[2]), Paths.get(args[3]), Paths.get(args[4]), args[5], Double.parseDouble(args[6]), args[7].trim());
        }
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

    private static StringBuilder handle_search_hit(final List<String> searchHit, final NttNmc nttNmc, final PepScore pepScore, final int oldRank, final int newRank) {
        if (nttNmc == null || pepScore == null) {
            return new StringBuilder();
        }

        final StringBuilder sb = new StringBuilder();
        double calc_neutral_pep_mass = Double.NaN;
        double massdiff = Double.NaN;
        int isomassd = 0;
        final Iterator<String> iterator = searchHit.iterator();
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
        sb.append(oldRank == newRank ? search_hit_line : search_hit_line.replace("hit_rank=\"" + oldRank + "\"", "hit_rank=\"" + newRank + "\"")).append("\n");
        String line;
        while (!(line = iterator.next()).trim().contentEquals("</search_hit>")) {
            sb.append(line).append("\n");
        }

        if (!Float.isNaN(nttNmc.spectralSimilarity)) {
            sb.append(String.format("<search_score name=\"spectralsim\" value=\"%f\"/>\n", nttNmc.spectralSimilarity));
        }
        if (!Float.isNaN(nttNmc.RTscore)) {
            sb.append(String.format("<search_score name=\"rtscore\" value=\"%f\"/>\n", nttNmc.RTscore));
        }
        if (!Float.isNaN(nttNmc.IMscore)) {
            sb.append(String.format("<search_score name=\"imscore\" value=\"%f\"/>\n", nttNmc.IMscore));
        }
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
                        1 - pepScore.pep, 1 - pepScore.pep, 1 - pepScore.pep, 1 - pepScore.pep,
                        pepScore.score, nttNmc.ntt, nttNmc.nmc, (massdiff - isomassd * 1.0033548378) * 1e6 / calc_neutral_pep_mass, isomassd
                ));
        sb.append("</search_hit>\n");
        return sb;
    }

    private static String handle_spectrum_query(final List<String> sq, final Map<String, NttNmc[]> pinSpectrumRankNttNmc, final Map<String, PepScore[]> pinSpectrumRankPepScore, final boolean is_DIA, final int DIA_rank) {
        final List<List<String>> search_hits = new ArrayList<>();
        final StringBuilder sb = new StringBuilder();
        String spectrum;
        final Iterator<String> iterator = sq.iterator();
        for (String line; iterator.hasNext(); ) {
            line = iterator.next().trim();
            spectrum = getSpectrum(line);

            final PepScore[] pepScoreArray = pinSpectrumRankPepScore.get(spectrum);
            if (pepScoreArray == null) {
                return "";
            }

            final NttNmc[] nttNmcArray = pinSpectrumRankNttNmc.get(spectrum);
            if (nttNmcArray == null) {
                return "";
            }

            if (is_DIA && (nttNmcArray[DIA_rank - 1] == null || pepScoreArray[DIA_rank - 1] == null)) {
                return "";
            }

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
                    if (is_DIA) // FixMe: it does not reorder the hits according to ranks updated by Percolator.
                        sb.append(handle_search_hit(search_hits.get(0), nttNmcArray[DIA_rank - 1], pepScoreArray[DIA_rank - 1], 1, 1));
                    else {
                        // write the search_hits ordered by Percolator
                        final TreeMap<Double, Integer> scoreOldRankMinusOne = new TreeMap<>(Collections.reverseOrder());
                        for (int oldRankMinusOne = 0; oldRankMinusOne < pepScoreArray.length; ++oldRankMinusOne) {
                            final PepScore pepScore = pepScoreArray[oldRankMinusOne];
                            if (pepScore == null) {
                                continue;
                            }
                            scoreOldRankMinusOne.put(pepScore.score, oldRankMinusOne);
                        }
                        int newRank = 0;
                        for (final Map.Entry<Double, Integer> entry : scoreOldRankMinusOne.entrySet()) {
                            final int oldRankMinusOne = entry.getValue();
                            sb.append(handle_search_hit(search_hits.get(oldRankMinusOne), nttNmcArray[oldRankMinusOne], pepScoreArray[oldRankMinusOne], oldRankMinusOne + 1, ++newRank));
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

    public static void percolatorToPepXML(final Path pin, final String basename, final Path percolatorTargetPsms, final Path percolatorDecoyPsms, final Path outBasename, final String DIA_DDA, final double minProb, String lcmsPath) {
        // Check if the LCMS files exist. Replace the non-existing ones with the existing ones if possible.
        if (!Files.exists(Paths.get(lcmsPath))) { // Try to find the alternative file.
            boolean notOk = true;
            if (lcmsPath.toLowerCase().endsWith("_calibrated.mzml")) {
                String ss = lcmsPath.substring(0, lcmsPath.length() - "_calibrated.mzml".length()) + ".mzML";
                if (Files.exists(Paths.get(ss))) {
                    lcmsPath = ss;
                    notOk = false;
                } else {
                    ss = lcmsPath.substring(0, lcmsPath.length() - "_calibrated.mzml".length()) + "_uncalibrated.mzML";
                    if (Files.exists(Paths.get(ss))) {
                        lcmsPath = ss;
                        notOk = false;
                    }
                }
            } else if (lcmsPath.toLowerCase().endsWith("_uncalibrated.mzml")) {
                String ss = lcmsPath.substring(0, lcmsPath.length() - "_uncalibrated.mzml".length()) + ".mzML";
                if (Files.exists(Paths.get(ss))) {
                    lcmsPath = ss;
                    notOk = false;
                } else {
                    ss = lcmsPath.substring(0, lcmsPath.length() - "_uncalibrated.mzml".length()) + "_calibrated.mzML";
                    if (Files.exists(Paths.get(ss))) {
                        lcmsPath = ss;
                        notOk = false;
                    }
                }
            }
            if (notOk) {
                System.err.printf(lcmsPath + " does not exist.");
                System.exit(1);
            }
        }

        // get max rank from pin
        final boolean is_DIA = DIA_DDA.equals("DIA");
        final int max_rank = get_max_rank(basename, is_DIA);
        if (max_rank < 1) {
            System.err.println("Cannot find output_report_topN parameter from " + basename + "'s pepXML file.");
            System.exit(1);
        }

        final Map<String, NttNmc[]> pinSpectrumRankNttNmc = new HashMap<>();
        final Map<String, PepScore[]> pinSpectrumRankPepScore = new HashMap<>();

        try {
            BufferedReader brtsv = Files.newBufferedReader(pin);
            final String pin_header = brtsv.readLine();
            if (pin_header == null) {
                throw new NullPointerException("Could not read the first line of " + pin.toAbsolutePath() + ".");
            }
            final List<String> colnames = Arrays.asList(pin_header.split("\t"));
            final int indexOf_SpecId = colnames.indexOf("SpecId");
            final int indexOf_ntt = colnames.indexOf("ntt");
            final int indexOf_nmc = colnames.indexOf("nmc");
            final int indexOf_expRT = colnames.indexOf("retentiontime");
            int indexOf_spectralSimilarity = -1;
            int indexOf_predRT = -1;
            int indexOf_IMscore = -1;
            if (colnames.contains("bray_curtis")) { //will need to adjust in future, if more scores are allowed
                indexOf_spectralSimilarity = colnames.indexOf("bray_curtis");
            }
            if (colnames.contains("unweighted_spectral_entropy")) {
                indexOf_spectralSimilarity = colnames.indexOf("unweighted_spectral_entropy");
            }
            if (colnames.contains("delta_RT_loess")) {
                indexOf_predRT = colnames.indexOf("pred_RT_real_units");
            }
            if (colnames.contains("delta_IM_loess")) {
                indexOf_IMscore = colnames.indexOf("delta_IM_loess");
            }
            String line;

            while ((line = brtsv.readLine()) != null) {
                final String[] split = line.split("\t");
                final String raw_SpecId = split[indexOf_SpecId];
                final Spectrum_rank spectrum_rank = get_spectrum_rank(raw_SpecId);
                final String specId = spectrum_rank.spectrum;
                final int rank = spectrum_rank.rank;
                final int ntt = Integer.parseInt(split[indexOf_ntt]);
                final int nmc = Integer.parseInt(split[indexOf_nmc]);
                float spectralSimilarity = Float.NaN;
                if (indexOf_spectralSimilarity != -1) {
                    spectralSimilarity = Float.parseFloat(split[indexOf_spectralSimilarity]);
                }
                float RTscore = Float.NaN;
                if (indexOf_predRT != -1) {
                    RTscore = Math.abs(Float.parseFloat(split[indexOf_predRT]) -
                            Float.parseFloat(split[indexOf_expRT]));
                }
                float IMscore = Float.NaN;
                if (indexOf_IMscore != -1) {
                    IMscore = Float.parseFloat(split[indexOf_IMscore]);
                }
                pinSpectrumRankNttNmc.computeIfAbsent(specId, e -> new NttNmc[max_rank])[rank - 1] =
                        new NttNmc(ntt, nmc, spectralSimilarity, RTscore, IMscore);
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
                    final String specId = spectrum_rank.spectrum;
                    final int rank = spectrum_rank.rank;
                    double pep;
                    try {
                        pep = Double.parseDouble(split[indexOfPEP]);
                    } catch (NumberFormatException e) {
                        pep = 1.0;
                    }

                    if (1 - pep < minProb) {
                        continue;
                    }

                    double score;
                    try {
                        score = Double.parseDouble(split[indexOfScore]);
                    } catch (NumberFormatException e) {
                        score = 0.0;
                    }

                    pinSpectrumRankPepScore.computeIfAbsent(specId, e -> new PepScore[max_rank])[rank - 1] = new PepScore(pep, score);
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

            try (final BufferedReader brpepxml = Files.newBufferedReader(pepxml_rank);
                 final BufferedWriter out = Files.newBufferedWriter(output_rank)) {
                String line;
                while ((line = brpepxml.readLine()) != null) {
                    if (line.trim().startsWith("<msms_run_summary")) {
                        Matcher matcher1 = pattern1.matcher(line);
                        if (matcher1.find()) {
                            line = matcher1.replaceFirst(Matcher.quoteReplacement("base_name=\"" + StringUtils.upToLastDot(lcmsPath) + "\""));

                            Matcher matcher2 = pattern2.matcher(line);
                            if (matcher2.find()) {
                                line = matcher2.replaceFirst("raw_data_type=\"" + StringUtils.afterLastDot(lcmsPath) + "\"");
                            }

                            Matcher matcher3 = pattern3.matcher(line);
                            if (matcher3.find()) {
                                line = matcher3.replaceFirst("raw_data=\"" + StringUtils.afterLastDot(lcmsPath) + "\"");
                            }
                        } else {
                            System.err.printf("Could not find the base_name from " + pepxml_rank);
                            System.exit(1);
                        }
                    }

                    out.write(line + "\n");

                    if (line.trim().startsWith("<msms_pipeline_analysis ")) {
                        final String now = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(LocalDateTime.now());
                        final String tmp = String.format(
                                "<analysis_summary analysis=\"Percolator\" time=\"%s\">\n" +
                                "<peptideprophet_summary min_prob=\"%.2f\">\n" +
                                "<inputfile name=\"%s\"/>\n" +
                                "</peptideprophet_summary>\n" +
                                "</analysis_summary>\n" +
                                "<analysis_summary analysis=\"database_refresh\" time=\"%s\"/>\n" +
                                "<analysis_summary analysis=\"interact\" time=\"%s\">\n" +
                                "<interact_summary filename=\"%s\" directory=\"\">\n" +
                                "<inputfile name=\"%s\"/>\n" +
                                "</interact_summary>\n" +
                                "</analysis_summary>\n" +
                                "<dataset_derivation generation_no=\"0\"/>\n",
                                now, minProb, pepxml_rank.toAbsolutePath(), now, now, output_rank.toAbsolutePath(), pepxml_rank.toAbsolutePath());
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
                                out.write(handle_spectrum_query(sq, pinSpectrumRankNttNmc, pinSpectrumRankPepScore, is_DIA, rank));
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


    static class NttNmc {

        final int ntt;
        final int nmc;
        final float spectralSimilarity;
        final float RTscore;
        final float IMscore;

        public NttNmc(int ntt, int nmc, float spectralSimilarity, float RTscore, float IMscore) {
            this.ntt = ntt;
            this.nmc = nmc;
            this.spectralSimilarity = spectralSimilarity;
            this.RTscore = RTscore;
            this.IMscore = IMscore;
        }
    }


    static class PepScore {

        final double pep;
        final double score;

        public PepScore(double pep, double score) {
            this.pep = pep;
            this.score = score;
        }
    }
}
