/*
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.dmtavt.fragpipe.params.philosopher;

import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import com.github.chhh.utils.PropertiesUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PhilosopherProps {
    public static final String PROGRAM_NAME = "Philosopher";
    public static final String CMD_COMET = "comet";
    public static final String CMD_PEPTIDE_PROPHET = "peptideprophet";
    public static final String CMD_PROTEIN_PROPHET = "proteinprophet";
    public static final String CMD_DATABASE = "database";
    public static final String CMD_FILTER = "filter";
    public static final String CMD_REPORT = "report";
    public static final String CMD_IPROPHET = "iprophet";
    public static final String CMD_LABELFREE = "freequant";
    public static final String CMD_LABELQUANT = "labelquant";

    private static final String PROPERTIES_FILE_NAME = "philosopher.properties";

    public static final String PROP_LATEST_COMPATIBLE_VERSION = "philosopher.version.latest-compatible";
    public static final String PROP_LOWEST_COMPATIBLE_VERSION = "philosopher.version.lowest-compatible";
    public static final String PROP_DOWNLOAD_URL = "philosopher.download.url";

    public static final List<String> PROPERTIES_URLS = Arrays.asList(
        "https://raw.githubusercontent.com/Nesvilab/FragPipe/master/MSFragger-GUI/src/umich/msfragger/params/philosopher/philosopher.properties"
        //"https://raw.githubusercontent.com/chhh/FragPipe/updates/MSFragger-GUI/src/umich/msfragger/params/philosopher/philosopher.properties",
        //"https://raw.githubusercontent.com/chhh/FragPipe/master/MSFragger-GUI/src/umich/msfragger/params/philosopher/philosopher.properties"
    );

    private static class Holder {
        private static final Properties properties = PropertiesUtils
            .initProperties(PROPERTIES_URLS, PROPERTIES_FILE_NAME, PhilosopherProps.class);
        public static Properties getProperties() {
            return properties;
        }
    }

    public static Properties getProperties() {
        return Holder.getProperties();
    }
}
