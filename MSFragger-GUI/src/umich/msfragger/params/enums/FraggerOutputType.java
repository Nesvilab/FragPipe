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
package umich.msfragger.params.enums;

import umich.msfragger.params.fragger.MsfraggerParams;

/**
 *
 * @author Dmitry Avtonomov
 */
public enum FraggerOutputType {
    
    PEP_XML("pepXML", "pepXML"),
    TSV("tsv", "tsv"),
    TSV_AND_PEPXML("pepXML", "tsv_pepXML"),
    PIN("pin", "pin");

    
    String extension;
    String valueInParamsFile;

    private FraggerOutputType(String extension, String valueInParamsFile) {
        this.extension = extension;
        this.valueInParamsFile = valueInParamsFile;
    }
    
    public String valueInParamsFile() {
        return valueInParamsFile;
    }

    public String getExtension() {
        return extension;
    }

    public static FraggerOutputType fromValueInParamsFile(String val) {
        for (FraggerOutputType t : FraggerOutputType.values()) {
            if (t.valueInParamsFile().equalsIgnoreCase(val))
                return t;
        }
        throw new IllegalStateException("Unknown output format stored in properties (property '"
            + MsfraggerParams.PROP_output_format + "')");
    }
}
