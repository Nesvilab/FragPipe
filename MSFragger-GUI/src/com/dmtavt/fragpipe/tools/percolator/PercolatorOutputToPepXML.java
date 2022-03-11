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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tools.percolator;

import org.jooq.lambda.function.Consumer3;

import javax.xml.stream.XMLStreamException;
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
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PercolatorOutputToPepXML {
    private static final boolean isDebug = true;

    private static final Pattern reWhitespaces = Pattern.compile("\\s+");
    private static final Pattern reSpecQuerySpectrum = Pattern.compile("(.+spectrum=\\s*\"[^\"]+\\.)([0-9]+)\\.([0-9]+)(\\.[0-9]+\".+)");
    /**
     * Split an example like:<br/>
     * {@code C:\Human-Protein-Training_Trypsin_821_3_2} into 3 groups<br/>
     * {@code C:\[Human-Protein-Training_Trypsin]_[821]_3_[2]}
     */
    private static final Pattern reSpecRankPinComet = Pattern.compile("([^\\\\/]+_\\d+)_\\d+_(\\d+)$");
    /**
     * Split an example like:<br/>
     * {@code Human-Protein-Training_Trypsin.12198.12198.4_1} into 2 groups<br/>
     * {@code [Human-Protein-Training_Trypsin.12198.12198].4_[1]}
     */
    private static final Pattern reSpecRankPinMsFragger = Pattern.compile("([^\\\\/]+)\\.\\d+_(\\d+)$");
    private static Pattern rePepxmlSpecId = Pattern.compile("([^\\\\/]+)\\.(0*(\\d+))\\.\\d+$");;
//    private static final Pattern reSpecRankPercolator = Pattern.compile("([^\\\\/]+)_\\d+_(\\d+)$");

    public static void main(final String[] args) {
        Locale.setDefault(Locale.US);
        if (args.length == 0) {
            percolatorToPepXML(
                Paths.get("F:\\dev\\msfragger\\msfraggerdia_old\\20190206_LUM1_CPBA_EASY04_060_30_SA_90mingrad_80B_DIA_400_1000_8mzol_15k_20IIT_4e5agc_1633-01_01.pin"),
                "F:\\dev\\msfragger\\msfraggerdia_old\\20190206_LUM1_CPBA_EASY04_060_30_SA_90mingrad_80B_DIA_400_1000_8mzol_15k_20IIT_4e5agc_1633-01_01",
                Paths.get("F:\\dev\\msfragger\\msfraggerdia_old\\20190206_LUM1_CPBA_EASY04_060_30_SA_90mingrad_80B_DIA_400_1000_8mzol_15k_20IIT_4e5agc_1633-01_01_percolator_target_psms.tsv"),
                Paths.get("F:\\dev\\msfragger\\msfraggerdia_old\\20190206_LUM1_CPBA_EASY04_060_30_SA_90mingrad_80B_DIA_400_1000_8mzol_15k_20IIT_4e5agc_1633-01_01_percolator_decoy_psms.tsv"),
                Paths.get("F:\\dev\\msfragger\\msfraggerdia_old\\interact-20190206_LUM1_CPBA_EASY04_060_30_SA_90mingrad_80B_DIA_400_1000_8mzol_15k_20IIT_4e5agc_1633-01_01"),
                "DIA",
                0);
        } else if (Files.exists(Paths.get(args[0].replace(".pin", "_edited.pin")))){
                percolatorToPepXML(Paths.get(args[0].replace(".pin", "_edited.pin")), args[1], Paths.get(args[2]), Paths.get(args[3]), Paths.get(args[4]), args[5], Double.parseDouble(args[6]));
        } else {
            percolatorToPepXML(Paths.get(args[0]), args[1], Paths.get(args[2]), Paths.get(args[3]), Paths.get(args[4]), args[5], Double.parseDouble(args[6]));
        }
    }

    private static String parsePepxmlSpecIdFromSpectrumQuery(final String line) {
        String spectrum = null;
        for (final String e : line.split("\\s"))
            if (e.startsWith("spectrum=")) {
                spectrum = e.substring("spectrum=\"".length(), e.length() - 1);
                break;
            }
        return spectrum.substring(0, spectrum.lastIndexOf("."));
    }

    /**
     * Prepends zeros to scan numbers in 'spectrum' attribute of 'spectrum_query' tag.
     */
    private static String prependZerosToSpectrumAttr(final String spectrumQueryLine) {
        Matcher matcher = reSpecQuerySpectrum.matcher(spectrumQueryLine);
        if (!matcher.matches()) {
            throw new RuntimeException("Cannot parse spectrum query line: " + spectrumQueryLine);
        }
        if (!matcher.group(2).contentEquals(matcher.group(3))) {
            throw new RuntimeException("Unexpected spectrum ID: " + spectrumQueryLine);
        }
        final String scanNum = matcher.group(2);
        final int minScanNumLen = 5;
        if (scanNum.length() >= minScanNumLen) {
            return spectrumQueryLine;
        }
        StringBuilder sb = new StringBuilder(minScanNumLen);
        for (int i = 0; i < minScanNumLen - scanNum.length(); ++i) {
            sb.append("0");
        }
        sb.append(scanNum);
        final StringBuilder sbRes = new StringBuilder(matcher.group(1))
                .append(sb).append(".").append(sb).append(matcher.group(4));
        return sbRes.toString();
    }

    private static class Spectrum_rank {
        final String spectrum;
        final int rank;

        Spectrum_rank(String spectrum, int rank) {
            this.spectrum = spectrum;
            this.rank = rank;
        }

        @Override
        public String toString() {
            return "Spectrum_rank{" +
                    "spectrum='" + spectrum + '\'' +
                    ", rank=" + rank +
                    '}';
        }
    }

    /**
     * Example to be parsed: Human-Protein-Training_Trypsin.12198.12198.4_1
     */
    private static Spectrum_rank get_spectrum_rank_msfragger(final String s){
        Matcher m = reSpecRankPinMsFragger.matcher(s);
        if (!m.find())
            throw new IllegalStateException("Unexpected input string for parsing MsFragger's SpectrumRank from SpecId: " + s);
        return new Spectrum_rank(m.group(1), Integer.parseInt(m.group(2)));
    }

    /**
     * Example to be parsed: C:\Human-Protein-Training_Trypsin_821_3_2
     */
    private static Spectrum_rank get_spectrum_rank_comet(String s) {
        Matcher m = reSpecRankPinComet.matcher(s);
        if (!m.find())
            throw new IllegalStateException("Unexpected input string for parsing Comet's SpectrumRank from SpecId: " + s);
        return new Spectrum_rank(m.group(1), Integer.parseInt(m.group(2)));
    }

    private static List<StoxParserPepxml.NameValue> parseParamsFromPepxml(Path pepxmlPath) throws XMLStreamException, IOException {
        List<StoxParserPepxml.NameValue> params = StoxParserPepxml.parseSearchSummary(pepxmlPath);
        return params;
    }

    private static int findMaxRank(List<StoxParserPepxml.NameValue> params) {
        for (StoxParserPepxml.NameValue kv : params) {
            if ("output_report_topN".equals(kv.k)           // MsFragger
                    || "num_output_lines".equals(kv.k)) {   // Comet
                return Integer.parseInt(kv.v);
            }
        }
        throw new IllegalStateException("Did not find `output_report_topN` or `num_output_lines` search engine parameters");
    }


    private static StringBuilder handle_search_hit(final List<String> searchHit, final NttNmc nttNmc, final PepScore pepScore, final int oldRank, final int newRank) {
        final StringBuilder sb = new StringBuilder();
        if (nttNmc == null || pepScore == null) {
            return sb;
        }
        if (searchHit.isEmpty()) {
            return sb;
        }

        final Iterator<String> itSearchHit = searchHit.iterator();
        final String search_hit_line = itSearchHit.next();

        double massdiff = Double.NaN;
        double calc_neutral_pep_mass = Double.NaN;
        for (final String e : reWhitespaces.split(search_hit_line)) { // fixme: the code assumes that all attributes are in one line, which makes it not robust
            if (e.startsWith("massdiff="))
                massdiff = Double.parseDouble(e.substring("massdiff=\"".length(), e.length() - 1));
            if (e.startsWith("calc_neutral_pep_mass="))
                calc_neutral_pep_mass = Double.parseDouble(e.substring("calc_neutral_pep_mass=\"".length(), e.length() - 1));
        }

        int isomassd = computeIsoMassDiff(massdiff);

        String searchHitLineRankUpdated = search_hit_line;
        if (oldRank != newRank) {
            searchHitLineRankUpdated = search_hit_line.replace("hit_rank=\"" + oldRank + "\"", "hit_rank=\"" + newRank + "\"");
            //System.out.printf("Hit rank updated: %d->%d [%s]\n", oldRank, newRank, searchHit.get(0));
        }
        sb.append(searchHitLineRankUpdated).append("\n");

        String line;
        while (!(line = itSearchHit.next()).trim().startsWith("</search_hit>")) {
            sb.append(line).append("\n");
        }

        if (!Float.isNaN(nttNmc.spectralSimilarity)) {
            sb.append(String.format("<search_score name=\"spectralsim\" value=\"%f\"/>\n", nttNmc.spectralSimilarity));
        }
        if (!Float.isNaN(nttNmc.RTscore)) {
            sb.append(String.format("<search_score name=\"rtscore\" value=\"%f\"/>\n", nttNmc.RTscore));
        }

        final String analysisResult = createXmlAnalysisResultForSearchHit(nttNmc, pepScore, calc_neutral_pep_mass, massdiff, isomassd);
        sb.append(analysisResult);
        sb.append("</search_hit>\n");
        return sb;
    }

    private static int computeIsoMassDiff(double massdiff) {
        int isomassd = 0;
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
        return isomassd;
    }

    private static String createXmlAnalysisResultForSearchHit(NttNmc nttNmc, PepScore pepScore, double calc_neutral_pep_mass, double massdiff, int isomassd) {
        return String.format(
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
        );
    }

    private static String handle_spectrum_query_msfragger(final List<String> sq, final Map<String, NttNmc[]> pinSpectrumRankNttNmc, final Map<String, PepScore[]> pinSpectrumRankPepScore, final boolean is_DIA, final int DIA_rank) {
        final List<List<String>> search_hits = new ArrayList<>();
        final StringBuilder sb = new StringBuilder();
        final Iterator<String> itSpecQuery = sq.iterator();
        for (String line; itSpecQuery.hasNext(); ) {
            line = itSpecQuery.next().trim();
            final String spectrum = parsePepxmlSpecIdFromSpectrumQuery(line);

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

            sb.append(prependZerosToSpectrumAttr(line)).append('\n');

            while (itSpecQuery.hasNext()) { // fixme: the code assumes that there are always <search_hit, massdiff=, and calc_neutral_pep_mass=, which makes it not robust
                line = itSpecQuery.next().trim();
                if (line.contentEquals("<search_result>")) {
                    sb.append(line).append('\n');
                } else if (line.startsWith("<search_hit ")) {
                    final ArrayList<String> search_hit = new ArrayList<>();
                    search_hit.add(line);
                    do {
                        line = itSpecQuery.next().trim();
                        search_hit.add(line);
                    } while (!line.contentEquals("</search_hit>"));
                    search_hits.add(search_hit);
                } else if (line.contentEquals("</search_result>")) {
                    if (is_DIA) {// FixMe: it does not reorder the hits according to ranks updated by Percolator.
                        sb.append(handle_search_hit(search_hits.get(0), nttNmcArray[DIA_rank - 1], pepScoreArray[DIA_rank - 1], 1, 1));
                    } else {
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
                } else if (line.startsWith("</spectrum_query>")) {
                    sb.append(line).append('\n');
                } else {
                    throw new IllegalStateException("The loop did not end with </spectrum_query> line: " + line);
                }
            }
        }
        return sb.toString();
    }

    private static String handle_spectrum_query_comet(final List<String> sq, final Map<String, NttNmc[]> pinSpectrumRankNttNmc, final Map<String, PepScore[]> pinSpectrumRankPepScore, final boolean is_DIA, final int DIA_rank) {
        // check if there's at least one search hit (Comet result can have empty spectrum queries)
        boolean hasSearchHits = false;
        for (String sqLine : sq) {
            if (sqLine.trim().startsWith("<search_hit")) {
                hasSearchHits = true;
                break;
            }
        }
        if (!hasSearchHits) {
            return "";
        }

        final List<List<String>> search_hits = new ArrayList<>();
        final StringBuilder sb = new StringBuilder();
        final Iterator<String> itSpecQuery = sq.iterator();
        for (String line; itSpecQuery.hasNext(); ) {
            line = itSpecQuery.next().trim();
            final String spectrum = parsePepxmlSpecIdFromSpectrumQuery(line);
            final String cometPinSpecId = convertPepxmlSpecIdToCometPinSpecId(spectrum);

            final PepScore[] pepScoreArray = pinSpectrumRankPepScore.get(cometPinSpecId);
            if (pepScoreArray == null) {
                return "";
            }

            final NttNmc[] nttNmcArray = pinSpectrumRankNttNmc.get(cometPinSpecId);
            if (nttNmcArray == null) {
                return "";
            }

            if (is_DIA && (nttNmcArray[DIA_rank - 1] == null || pepScoreArray[DIA_rank - 1] == null)) {
                return "";
            }

            sb.append(prependZerosToSpectrumAttr(line)).append('\n');

            final Pattern reFullSearchResultStart = Pattern.compile("^\\s*<search_result>$");
            final Pattern reFullSearchResultEnd = Pattern.compile("^\\s*</search_result>$");
            final Pattern reStartsWithSearchHitStart = Pattern.compile("^\\s*<search_hit");
            final Pattern reFullSearchHitEnd = Pattern.compile("^\\s*</search_hit>");
            final Pattern reFullSpectrumQueryEnd = Pattern.compile("^\\s*</spectrum_query>$");

            while (itSpecQuery.hasNext()) { // fixme: the code assumes that there are always <search_hit, massdiff=, and calc_neutral_pep_mass=, which makes it not robust
                line = itSpecQuery.next().trim();

                if (reFullSearchResultStart.matcher(line).find()) {
                    sb.append(line).append('\n');

                } else if (reStartsWithSearchHitStart.matcher(line).find()) {
                    final ArrayList<String> search_hit = new ArrayList<>();
                    search_hit.add(line);
                    do {
                        if (!itSpecQuery.hasNext()) {
                            throw new IllegalStateException("Unexpected to have no lines within a 'search_hit'");
                        }
                        line = itSpecQuery.next().trim();
                        search_hit.add(line);
                    } while (!line.contentEquals("</search_hit>"));
                    search_hits.add(search_hit);

                } else if (reFullSearchResultEnd.matcher(line).find()) {
                    if (is_DIA) {// FixMe: it does not reorder the hits according to ranks updated by Percolator.
                        sb.append(handle_search_hit(search_hits.get(0), nttNmcArray[DIA_rank - 1], pepScoreArray[DIA_rank - 1], 1, 1));
                    } else {
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

                } else if (reFullSpectrumQueryEnd.matcher(line).find()) {
                    sb.append(line).append('\n');

                } else {
                    throw new IllegalStateException("The loop did not end with </spectrum_query> line: " + line);
                }
            }
        }
        return sb.toString();
    }

    /** Human-Protein-Training_Trypsin.00821.00821 -> Human-Protein-Training_Trypsin_00821 */
    private static String convertPepxmlSpecIdToCometPinSpecId(String spectrum) {
        Matcher m = rePepxmlSpecId.matcher(spectrum);
        if (!m.find())
            throw new IllegalStateException(String.format(
                    "Unexpected Pepxml spectrum ID [%s] doesn't match regexp [%s]",
                    spectrum, rePepxmlSpecId.pattern()));
        return m.group(1) + "_" + m.group(3);
    }

    private static void exit(String message, Throwable ex) {
        if (message != null)
            System.err.println(message);
        if (ex != null)
            ex.printStackTrace();
        System.exit(1);
    }
    private static void exit(String message) {
        exit(message, null);
    }

    public enum SearchEngine {Unknown, MsFragger, Comet}

    public static void percolatorToPepXML(final Path pin, final String basename, final Path percolatorTargetPsms,
                                          final Path percolatorDecoyPsms, final Path outBasename, final String DIA_DDA, final double minProb) {
        // get max rank from pin
        final boolean is_DIA = DIA_DDA.equals("DIA");
        final Path pathPepxml = is_DIA
                ? Paths.get(basename + "_rank1.pepXML")
                : Paths.get(basename + ".pepXML");

        List<StoxParserPepxml.NameValue> nameValues = null;
        try {
            nameValues = parseParamsFromPepxml(pathPepxml);
        } catch (XMLStreamException | IOException e) {
            exit("Couldn't parse search parameters from pepXML");
        }

        final int max_rank = findMaxRank(nameValues);
        if (max_rank < 1) {
            exit("Couldn't find max reported peptide rank in pepxml search engine parameters");
        }

        final SearchEngine se = findSearchEngine(nameValues);
        Function<String, Spectrum_rank> extractSpecRank;
        switch (se) {
            case MsFragger:
                extractSpecRank = PercolatorOutputToPepXML::get_spectrum_rank_msfragger;
                break;
            case Comet:
                extractSpecRank = PercolatorOutputToPepXML::get_spectrum_rank_comet;
                break;
            default:
                throw new UnsupportedOperationException("Only know how to extract SpectrumRank for Comet and MsFragger");
        }

        final Map<String, NttNmc[]> pinSpectrumRankNttNmc = new HashMap<>();
        parsePinAsRankNttNmc(pin, max_rank, pinSpectrumRankNttNmc, extractSpecRank);

        final Map<String, PepScore[]> pinSpectrumRankPepScore = new HashMap<>();
        parsePercolatorTsvAsRankPepScore(percolatorTargetPsms, percolatorDecoyPsms, minProb, max_rank, pinSpectrumRankPepScore, extractSpecRank);

        for (int rank = 1; rank <= (is_DIA ? max_rank : 1); ++rank) {
            final Path output_rank = is_DIA
                    ? Paths.get(outBasename + "_rank" + rank + ".pep.xml")
                    : Paths.get(outBasename + ".pep.xml");
            final Path pepxml_rank = is_DIA
                    ? Paths.get(basename + "_rank" + rank + ".pepXML")
                    : Paths.get(basename + ".pepXML");
            // fixme: cannot parse XML line-by-line because line break is allowed everywhere, including within an attribute, in a XML. Need to parse it using JDOM or JAXB
            createInteractFile(se, minProb, is_DIA, pinSpectrumRankNttNmc, pinSpectrumRankPepScore, rank, output_rank, pepxml_rank);
        }
    }

    private static void createInteractFile(SearchEngine se, double minProb, boolean is_DIA, Map<String, NttNmc[]> pinSpectrumRankNttNmc, Map<String, PepScore[]> pinSpectrumRankPepScore, int rank, Path output_rank, Path pepxml_rank) {
        HandleSpectrumQuery handleSpectrumQuery = null;
        switch (se) {
            case Unknown:
                throw new UnsupportedOperationException("Only know how to handle spectrum queries for pepXML from MsFragger and Comet");
            case MsFragger:
                handleSpectrumQuery = PercolatorOutputToPepXML::handle_spectrum_query_msfragger;
                break;
            case Comet:
                handleSpectrumQuery = PercolatorOutputToPepXML::handle_spectrum_query_comet;
                break;
        }

        try (final BufferedReader brpepxml = Files.newBufferedReader(pepxml_rank);
             final BufferedWriter out = Files.newBufferedWriter(output_rank)) {
            String line;
            while ((line = brpepxml.readLine()) != null) {
                out.write(line + "\n");
                if (line.trim().startsWith("<msms_pipeline_analysis ")) {
                    final String now = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(LocalDateTime.now());
                    writeAnalysisSummary(minProb, output_rank, pepxml_rank, out, now);
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
                            final String sqUpdated = handleSpectrumQuery.handle(sq, pinSpectrumRankNttNmc, pinSpectrumRankPepScore, is_DIA, rank);
                            out.write(sqUpdated);
                            break;
                        }
                    }
                }
            }
            out.write("</msms_run_summary>\n" +
                    "</msms_pipeline_analysis>");
        } catch (IOException e) {
            throw new UncheckedIOException("Error while creating interact file", e);
        }
    }

    @FunctionalInterface
    private static interface HandleSpectrumQuery {
        String handle(final List<String> sq, final Map<String, NttNmc[]> pinSpectrumRankNttNmc, final Map<String, PepScore[]> pinSpectrumRankPepScore, final boolean is_DIA, final int DIA_rank);
    }

    private static void writeAnalysisSummary(double minProb, Path output_rank, Path pepxml_rank, BufferedWriter out, String now) throws IOException {
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

    private static void debugPrint(String m) {
        if (isDebug)
            System.out.println(m);
    }

    /** Looks for 'search_summary'->'parameter' with search engine name. */
    private static SearchEngine findSearchEngine(List<StoxParserPepxml.NameValue> nameValues) {
        for (StoxParserPepxml.NameValue nv : nameValues) {
            if (nv.k.toLowerCase(Locale.ROOT).contains("msfragger"))
                return SearchEngine.MsFragger;
            if (nv.k.toLowerCase(Locale.ROOT).contains("comet"))
                return SearchEngine.Comet;
        }
        exit("Could not determine search engine from pepXML file header");
        return SearchEngine.Unknown; // should never get here, but static analysis complains
    }

    private static void parsePercolatorTsvAsRankPepScore(Path percolatorTargetPsms, Path percolatorDecoyPsms, double minProb, int max_rank, Map<String, PepScore[]> pinSpectrumRankPepScore,
                                                         Function<String, Spectrum_rank> extractSpecRank) {
        for (final Path tsv : new Path[]{percolatorTargetPsms, percolatorDecoyPsms}) {
            try (final BufferedReader brtsv = Files.newBufferedReader(tsv)) {
                final String percolator_header = brtsv.readLine();
                final List<String> colnames = Arrays.asList(percolator_header.split("\t"));
                final int indexOfPSMId = colnames.indexOf("PSMId");
                final int indexOfPEP = colnames.indexOf("posterior_error_prob");
                final int indexOfScore = colnames.indexOf("score");
                String line;
                int countSkipped = 0;
                int countTotal = 0;
                while ((line = brtsv.readLine()) != null) {
                    final String[] split = line.split("\t");
                    final String raw_psmid = split[indexOfPSMId];
                    final Spectrum_rank spectrum_rank = extractSpecRank.apply(raw_psmid);
                    final String specId = spectrum_rank.spectrum;
                    final int rank = spectrum_rank.rank;
                    final double pep = Double.parseDouble(split[indexOfPEP]);

                    countTotal += 1;
                    if (1 - pep < minProb) {
                        countSkipped += 1;
                        continue;
                    }

                    final double score = Double.parseDouble(split[indexOfScore]);
                    pinSpectrumRankPepScore.computeIfAbsent(specId, e -> new PepScore[max_rank])[rank - 1] = new PepScore(pep, score);
                }
                System.out.printf("Skipped %d/%d rows due to min probability cutoff %.4f in: %s\n", countSkipped, countTotal, minProb, tsv);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
    }

    private static class RtScoreMeanStdev {
        public final double mean;
        public final double stdev;
        public RtScoreMeanStdev(double mean, double stdev) {
            this.mean = mean;
            this.stdev = stdev;
        }
    }

    private static RtScoreMeanStdev computeRtScoreFromPin(Path pin, int indexOf_RTscore) {
        try (final BufferedReader brtsv = Files.newBufferedReader(pin)) {
            final String pin_header = brtsv.readLine();
            //get all RTscores so we can calculate z scores
            ArrayList<Double> RTscoresArrayList = new ArrayList<>();

            double scoreMean = 0;
            double scoreStd = 0;
            String line;
            while ((line = brtsv.readLine()) != null) {
                String[] split = line.split("\t");
                double score = Double.parseDouble(split[indexOf_RTscore]);
                RTscoresArrayList.add(score);
                scoreMean += score;
            }
            scoreMean /= RTscoresArrayList.size();
            for (double d : RTscoresArrayList) {
                scoreStd += Math.pow(scoreMean - d, 2) / RTscoresArrayList.size();
            }
            scoreStd = Math.sqrt(scoreStd);
            return new RtScoreMeanStdev(scoreMean, scoreStd);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static void parsePinAsRankNttNmc(Path pin, int max_rank, final Map<String, NttNmc[]> pinSpectrumRankNttNmc,
                                             Function<String, Spectrum_rank> extractSpecRank) {
        try (final BufferedReader brtsv = Files.newBufferedReader(pin)) {
            final String pin_header = brtsv.readLine();
            final List<String> colnames = Arrays.asList(pin_header.split("\t"));
            final int indexOf_SpecId = colnames.indexOf("SpecId");

            final int indexOf_ntt = colnames.indexOf("ntt");
            final int indexOf_nmc = colnames.indexOf("nmc");

            int indexOf_spectralSimilarity_tmp = colnames.indexOf("unweighted_spectral_entropy");
            if (indexOf_spectralSimilarity_tmp < 0)
                indexOf_spectralSimilarity_tmp = colnames.indexOf("bray_curtis");
            final int indexOf_spectralSimilarity = indexOf_spectralSimilarity_tmp;
            final int indexOf_RTscore = colnames.indexOf("delta_RT_loess");
            final RtScoreMeanStdev rtScoreMeanStdev = indexOf_RTscore < 0
                    ? null
                    : computeRtScoreFromPin(pin, indexOf_RTscore);

            Consumer3<String[], String, Integer> extractNttNmc = null;
            if (indexOf_ntt >= 0 && indexOf_nmc >= 0) {
                // MsFragger
                System.out.println("Looks to be MsFragger-compatible pin file");
                extractNttNmc = (split, specId, rank) -> {
                    final int ntt = Integer.parseInt(split[indexOf_ntt]);
                    final int nmc = Integer.parseInt(split[indexOf_nmc]);
                    float spectralSimilarity = Float.NaN;
                    if (indexOf_spectralSimilarity != -1) {
                        spectralSimilarity = Float.parseFloat(split[indexOf_spectralSimilarity]);
                    }
                    float RTscore = Float.NaN;
                    if (indexOf_RTscore != -1) {
                        final double scoreMean = rtScoreMeanStdev.mean;
                        final double scoreStd = rtScoreMeanStdev.stdev;
                        RTscore = (float) ((Double.parseDouble(split[indexOf_RTscore]) - scoreMean) / scoreStd);
                    }
                    pinSpectrumRankNttNmc.computeIfAbsent(specId,
                            e -> new NttNmc[max_rank])[rank - 1] = new NttNmc(ntt, nmc, spectralSimilarity, RTscore);
                };
            }
            final int indexOf_enzN = colnames.indexOf("enzN");
            final int indexOf_enzC = colnames.indexOf("enzC");
            final int indexOf_enzInt = colnames.indexOf("enzInt");
            if (indexOf_enzC >= 0 && indexOf_enzN >= 0 && indexOf_enzInt >= 0) {
                // Comet
                System.out.println("Looks to be Comet-compatible pin file");
                extractNttNmc = (split, specId, rank) -> {
                    final int enzN = Integer.parseInt(split[indexOf_enzN]);
                    final int enzC = Integer.parseInt(split[indexOf_enzC]);
                    final int nmc = Integer.parseInt(split[indexOf_enzInt]);
                    final int ntt = enzN + enzC;
                    // TODO: Does Comet case need that spectral similarity and RT similarity?
                    pinSpectrumRankNttNmc.computeIfAbsent(specId,
                            e -> new NttNmc[max_rank])[rank - 1] = new NttNmc(ntt, nmc, Float.NaN, Float.NaN);
                };
            }
            if (extractNttNmc == null) {
                throw new IllegalStateException("Did not find ntt/nmc or enzN/enzC/enzInt columns in the pin file");
            }

            String line;
            while ((line = brtsv.readLine()) != null) {
                final String[] split = line.split("\t");
                final String raw_SpecId = split[indexOf_SpecId];
                final Spectrum_rank spectrum_rank = extractSpecRank.apply(raw_SpecId);
                final String specId = spectrum_rank.spectrum;
                final int rank = spectrum_rank.rank;
                extractNttNmc.accept(split, specId, rank);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    static class NttNmc {

        final int ntt;
        final int nmc;
        final float spectralSimilarity;
        final float RTscore;

        public NttNmc(int ntt, int nmc, float spectralSimilarity, float RTscore) {
            this.ntt = ntt;
            this.nmc = nmc;
            this.spectralSimilarity = spectralSimilarity;
            this.RTscore = RTscore;
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
