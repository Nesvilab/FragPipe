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

package org.nesvilab.utils;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import okio.BufferedSource;
import okio.Okio;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.nesvilab.utils.FileUtils.FileSize;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IOUtilsTest {
  private static final Logger log = LoggerFactory.getLogger(IOUtilsTest.class);
  private static final String fnNoBom = "bom-none.txt";
  private static final String fnUtf8 = "bom-utf8.txt";
  private static final String fnUtf16Be = "bom-utf16be.txt";
  private static final String fnUtf16Le = "bom-utf16le.txt";

  @Test @Ignore("It's a dev/debug driver")
  public void tokenizeTest() throws IOException {
    //InputStream is = new ByteArrayInputStream(pepxmlSample().getBytes());

    Path path = PathUtils.existing("D:\\ms-data\\TMTIntegrator_v1.1.4\\TMT-I-Test\\03CPTAC_CCRCC_W_JHU_20171022\\03CPTAC_CCRCC_W_JHU_20171022_LUMOS_f03.mzML");
    FileSize fileSize = FileUtils.fileSize(path);
    log.debug("File ({}): {}", fileSize, path);

//    log.debug("tokienize 1 test started");
//    InputStream is = new FileInputStream(path.toFile());
//    IOUtils.tokenize1(is, "<", ">");
//    log.debug("tokienize 1 test done");

    log.debug("tokienize 2 test started");
    InputStream is = new FileInputStream(path.toFile());
    IOUtils.tokenize2(is, "<", ">");
    log.debug("tokienize 2 test done");
  }

  @Test
  public void pathDbDownload() {
    Path path = Paths.get("D:\\ms-data\\fasta\\2020-08-13-decoys-reviewed-contam-UP000005640");
    //boolean isRegularFile = Files.isRegularFile(path);
    boolean isNotInMeta = Arrays
        .stream(path.toAbsolutePath().normalize().toString().split("[\\\\/]"))
        .noneMatch(".meta"::equalsIgnoreCase);
    //Assert.assertTrue(isRegularFile);
    Assert.assertTrue(isNotInMeta);

    path = Paths.get("D:\\ms-data\\fasta\\.meta\\2020-08-13-decoys-reviewed-contam-UP000005640");
    isNotInMeta = Arrays
        .stream(path.toAbsolutePath().normalize().toString().split("[\\\\/]"))
        .noneMatch(".meta"::equalsIgnoreCase);
    Assert.assertFalse(isNotInMeta);
  }

  //@Test
  public void detectBomTest() throws Exception {
    Map<String, BOM> map = new LinkedHashMap<>();
    map.put(fnNoBom, BOM.NONE);
    map.put(fnUtf8, BOM.UTF_8);
    map.put(fnUtf16Be, BOM.UTF_16_BE);
    map.put(fnUtf16Le, BOM.UTF_16_LE);
    for (Entry<String, BOM> kv : map.entrySet()) {
      Path path = getResource(IOUtilsTest.class, "bom", kv.getKey());
      log.debug("BOM file path: {}", path);
      try (BufferedSource bs = Okio.buffer(Okio.source(path))) {
        BOM bom = IOUtils.detectBom(bs);
        Assert.assertEquals("Wrong BOM detected", kv.getValue(), bom);
      }
    }
  }

  public static Path getResource(Class<?> clazz, String resourceLocation, String resourceName)
      throws Exception {
    ClassLoader cl = clazz.getClassLoader();
    final URI uri = Objects.requireNonNull(cl.getResource(resourceLocation)).toURI();
    final Path path = Paths.get(uri).toAbsolutePath();
    return Paths.get(path.toString(), resourceName).toAbsolutePath();
  }

  private String pepxmlSample() {
    return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        + "<?xml-stylesheet type=\"text/xsl\" href=\"pepXML_std.xsl\"?>\n"
        + "<msms_pipeline_analysis date=\"2013:01:23:18:24:22\" summary_xml=\"\"\n"
        + "  xmlns=\"http://regis-web.systemsbiology.net/pepXML\"\n"
        + ">\n"
        + "  <msms_run_summary base_name=\"./output.2013_01_23_18_24_32.t.xml\" raw_data=\".?\"\n"
        + "    raw_data_type=\"raw\" search_engine=\"X! Tandem\">\n"
        + "    <sample_enzyme name=\"trypsin\">\n"
        + "      <specificity cut=\"KR\" no_cut=\"P\" sense=\"C\"/>\n"
        + "    </sample_enzyme>\n"
        + "    <search_summary base_name=\"./output.2013_01_23_18_24_32.t.xml\" fragment_mass_type=\"monoisotopic\"\n"
        + "      precursor_mass_type=\"monoisotopic\" search_engine=\"X! Tandem\" search_id=\"1\">\n"
        + "      <search_database local_path=\"/home/lab006/fasta/uniprot_sprot_with_decoy.fasta\" type=\"AA\"/>\n"
        + "      <enzymatic_search_constraint enzyme=\"trypsin\" max_num_internal_cleavages=\"1\"\n"
        + "        min_number_termini=\"2\"/>\n"
        + "      <aminoacid_modification aminoacid=\"C\" mass=\"160.0306\" massdiff=\"57.0215\" variable=\"N\"/>\n"
        + "      <aminoacid_modification aminoacid=\"C\" mass=\"143.0041\" massdiff=\"-17.0265\" symbol=\"^\"\n"
        + "        variable=\"Y\"/><!--X! Tandem n-terminal AA variable modification-->\n"
        + "      <aminoacid_modification aminoacid=\"E\" mass=\"111.0320\" massdiff=\"-18.0106\" symbol=\"^\"\n"
        + "        variable=\"Y\"/><!--X! Tandem n-terminal AA variable modification-->\n"
        + "      <aminoacid_modification aminoacid=\"Q\" mass=\"111.0321\" massdiff=\"-17.0265\" symbol=\"^\"\n"
        + "        variable=\"Y\"/><!--X! Tandem n-terminal AA variable modification-->\n"
        + "\n"
        + "      <!-- Input parameters -->\n"
        + "      <parameter name=\"list path, taxonomy information\" value=\"./taxonomy.xml\"/>\n"
        + "      <parameter name=\"output, histogram column width\" value=\"30\"/>\n"
        + "      <parameter name=\"output, histograms\" value=\"yes\"/>\n"
        + "      <parameter name=\"output, log path\" value=\"\"/>\n"
        + "      <parameter name=\"output, maximum valid expectation value\" value=\"0.1\"/>\n"
        + "      <parameter name=\"output, message\" value=\"testing 1 2 3\"/>\n"
        + "      <parameter name=\"output, one sequence copy\" value=\"yes\"/>\n"
        + "      <parameter name=\"output, parameters\" value=\"yes\"/>\n"
        + "      <parameter name=\"output, path\" value=\"./output.xml\"/>\n"
        + "      <parameter name=\"output, path hashing\" value=\"yes\"/>\n"
        + "      <parameter name=\"output, performance\" value=\"yes\"/>\n"
        + "      <parameter name=\"output, proteins\" value=\"yes\"/>\n"
        + "      <parameter name=\"output, results\" value=\"all\"/>\n"
        + "      <parameter name=\"output, sequence path\" value=\"\"/>\n"
        + "      <parameter name=\"output, sequences\" value=\"no\"/>\n"
        + "      <parameter name=\"output, sort results by\" value=\"protein\"/>\n"
        + "      <parameter name=\"output, spectra\" value=\"yes\"/>\n"
        + "      <parameter name=\"output, xsl path\" value=\"tandem-style.xsl\"/>\n"
        + "      <parameter name=\"protein, C-terminal residue modification mass\" value=\"0.0\"/>\n"
        + "      <parameter name=\"protein, N-terminal residue modification mass\" value=\"0.0\"/>\n"
        + "      <parameter name=\"protein, cleavage C-terminal mass change\" value=\"+17.002735\"/>\n"
        + "      <parameter name=\"protein, cleavage N-terminal mass change\" value=\"+1.007825\"/>\n"
        + "      <parameter name=\"protein, cleavage site\" value=\"[RK]|{P}\"/>\n"
        + "      <parameter name=\"protein, homolog management\" value=\"no\"/>\n"
        + "      <parameter name=\"protein, modified residue mass file\" value=\"\"/>\n"
        + "      <parameter name=\"protein, quick acetyl\" value=\"no\"/>\n"
        + "      <parameter name=\"protein, quick pyrolidone\" value=\"no\"/>\n"
        + "      <parameter name=\"protein, taxon\" value=\"python\"/>\n"
        + "      <parameter name=\"refine\" value=\"no\"/>\n"
        + "      <parameter name=\"refine, maximum valid expectation value\" value=\"0.1\"/>\n"
        + "      <parameter name=\"refine, sequence path\" value=\"\"/>\n"
        + "      <parameter name=\"refine, spectrum synthesis\" value=\"yes\"/>\n"
        + "      <parameter name=\"residue, modification mass\" value=\"57.021464@C\"/>\n"
        + "      <parameter name=\"residue, potential modification mass\" value=\"\"/>\n"
        + "      <parameter name=\"residue, potential modification motif\" value=\"\"/>\n"
        + "      <parameter name=\"scoring, a ions\" value=\"no\"/>\n"
        + "      <parameter name=\"scoring, b ions\" value=\"yes\"/>\n"
        + "      <parameter name=\"scoring, c ions\" value=\"no\"/>\n"
        + "      <parameter name=\"scoring, cyclic permutation\" value=\"no\"/>\n"
        + "      <parameter name=\"scoring, include reverse\" value=\"no\"/>\n"
        + "      <parameter name=\"scoring, maximum missed cleavage sites\" value=\"1\"/>\n"
        + "      <parameter name=\"scoring, minimum ion count\" value=\"1\"/>\n"
        + "      <parameter name=\"scoring, x ions\" value=\"no\"/>\n"
        + "      <parameter name=\"scoring, y ions\" value=\"yes\"/>\n"
        + "      <parameter name=\"scoring, z ions\" value=\"no\"/>\n"
        + "      <parameter name=\"spectrum, dynamic range\" value=\"100.0\"/>\n"
        + "      <parameter name=\"spectrum, fragment mass type\" value=\"monoisotopic\"/>\n"
        + "      <parameter name=\"spectrum, fragment monoisotopic mass error\" value=\"100\"/>\n"
        + "      <parameter name=\"spectrum, fragment monoisotopic mass error units\" value=\"ppm\"/>\n"
        + "      <parameter name=\"spectrum, maximum parent charge\" value=\"4\"/>\n"
        + "      <parameter name=\"spectrum, minimum fragment mz\" value=\"150.0\"/>\n"
        + "      <parameter name=\"spectrum, minimum parent m+h\" value=\"500.0\"/>\n"
        + "      <parameter name=\"spectrum, minimum peaks\" value=\"1\"/>\n"
        + "      <parameter name=\"spectrum, parent monoisotopic mass error minus\" value=\"10\"/>\n"
        + "      <parameter name=\"spectrum, parent monoisotopic mass error plus\" value=\"10\"/>\n"
        + "      <parameter name=\"spectrum, parent monoisotopic mass error units\" value=\"ppm\"/>\n"
        + "      <parameter name=\"spectrum, parent monoisotopic mass isotope error\" value=\"no \"/>\n"
        + "      <parameter name=\"spectrum, path\" value=\"example.mgf\"/>\n"
        + "      <parameter name=\"spectrum, sequence batch size\" value=\"1000\"/>\n"
        + "      <parameter name=\"spectrum, threads\" value=\"10\"/>\n"
        + "      <parameter name=\"spectrum, total peaks\" value=\"1000\"/>\n"
        + "      <parameter name=\"spectrum, use noise suppression\" value=\"no\"/>\n"
        + "      <!-- Unused input parameters -->\n"
        + "      <parameter name=\"protein, use minimal annotations\" value=\"yes\"/>\n"
        + "      <parameter name=\"refine, modification mass\" value=\"\"/>\n"
        + "      <parameter name=\"refine, point mutations\" value=\"no\"/>\n"
        + "      <parameter name=\"refine, potential C-terminus modifications\" value=\"\"/>\n"
        + "      <parameter name=\"refine, potential N-terminus modifications\" value=\"+42.010565@[\"/>\n"
        + "      <parameter name=\"refine, potential modification mass\" value=\"\"/>\n"
        + "      <parameter name=\"refine, potential modification motif\" value=\"\"/>\n"
        + "      <parameter name=\"refine, tic percent\" value=\"20\"/>\n"
        + "      <parameter name=\"refine, unanticipated cleavage\" value=\"yes\"/>\n"
        + "      <parameter name=\"refine, use potential modifications for full refinement\" value=\"no\"/>\n"
        + "      <parameter name=\"scoring, pluggable scoring\" value=\"no\"/>\n"
        + "      <!-- Performance parameters -->\n"
        + "      <parameter name=\"list path, sequence source #1\"\n"
        + "        value=\"/home/lab006/fasta/uniprot_sprot_with_decoy.fasta\"/>\n"
        + "      <parameter name=\"list path, sequence source description #1\" value=\"no description\"/>\n"
        + "      <parameter name=\"modelling, total peptides used\" value=\"3051\"/>\n"
        + "      <parameter name=\"modelling, total proteins used\" value=\"1077170\"/>\n"
        + "      <parameter name=\"modelling, total spectra used\" value=\"1\"/>\n"
        + "      <parameter name=\"process, start time\" value=\"2013:01:23:18:24:22\"/>\n"
        + "      <parameter name=\"process, version\" value=\"x! tandem CYCLONE (2012.10.01.1)\"/>\n"
        + "      <parameter name=\"quality values\" value=\"0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0\"/>\n"
        + "      <parameter name=\"refining, # input models\" value=\"0\"/>\n"
        + "      <parameter name=\"refining, # input spectra\" value=\"0\"/>\n"
        + "      <parameter name=\"refining, # partial cleavage\" value=\"0\"/>\n"
        + "      <parameter name=\"refining, # point mutations\" value=\"0\"/>\n"
        + "      <parameter name=\"refining, # potential C-terminii\" value=\"0\"/>\n"
        + "      <parameter name=\"refining, # potential N-terminii\" value=\"0\"/>\n"
        + "      <parameter name=\"refining, # unanticipated cleavage\" value=\"0\"/>\n"
        + "      <parameter name=\"timing, initial modelling total (sec)\" value=\"10.20\"/>\n"
        + "      <parameter name=\"timing, initial modelling/spectrum (sec)\" value=\"10.2000\"/>\n"
        + "      <parameter name=\"timing, load sequence models (sec)\" value=\"1.13\"/>\n"
        + "      <parameter name=\"timing, refinement/spectrum (sec)\" value=\"0.0000\"/>\n"
        + "    </search_summary>\n"
        + "\n"
        + "    <spectrum_query assumed_charge=\"2\" end_scan=\"1\" index=\"1\" precursor_neutral_mass=\"683.3966\"\n"
        + "      spectrum=\"\" start_scan=\"1\">\n"
        + "      <search_result>\n"
        + "        <search_hit calc_neutral_pep_mass=\"683.3966\" hit_rank=\"1\" is_rejected=\"0\" massdiff=\"0.000\"\n"
        + "          num_matched_ions=\"8\"\n"
        + "          num_missed_cleavages=\"0\"\n"
        + "          num_tol_term=\"2\" num_tot_proteins=\"1\" peptide=\"GPAAIQK\"\n"
        + "          peptide_next_aa=\"N\" peptide_prev_aa=\"K\" protein=\"sp|P35527|K1C9_HUMAN\"\n"
        + "          protein_descr=\"Keratin, type I cytoskeletal 9 OS=Homo sapiens GN=KRT9 PE=1 SV=3\" tot_num_ions=\"12\">\n"
        + "          <search_score name=\"hyperscore\" value=\"23.2\"/>\n"
        + "          <search_score name=\"nextscore\" value=\"17.3\"/>\n"
        + "          <search_score name=\"bscore\" value=\"6.6\"/>\n"
        + "          <search_score name=\"yscore\" value=\"12.1\"/>\n"
        + "          <search_score name=\"cscore\" value=\"0\"/>\n"
        + "          <search_score name=\"zscore\" value=\"0\"/>\n"
        + "          <search_score name=\"ascore\" value=\"0\"/>\n"
        + "          <search_score name=\"xscore\" value=\"0\"/>\n"
        + "          <search_score name=\"expect\" value=\"1.3\"/>\n"
        + "        </search_hit>\n"
        + "      </search_result>\n"
        + "    </spectrum_query>\n"
        + "  </msms_run_summary>\n"
        + "</msms_pipeline_analysis>\n";
  }
}