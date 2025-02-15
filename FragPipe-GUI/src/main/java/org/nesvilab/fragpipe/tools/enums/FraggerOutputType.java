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
package org.nesvilab.fragpipe.tools.enums;

import org.nesvilab.fragpipe.tools.fragger.MsfraggerParams;

/**
 *
 * @author Dmitry Avtonomov
 */
public enum FraggerOutputType {
    
    PEPXML("pepXML", "pepXML"),
    TSV("tsv", "tsv"),
    TSV_PEPXML("pepXML", "tsv_pepXML"),
    PIN("pin", "pin"),
    TSV_PIN("tsv", "tsv_pin"),
    PEPXML_PIN("pepXML", "pepXML_pin"),
    TSV_PEPXML_PIN("pepXML", "tsv_pepXML_pin");

    
    String extension;
    String valueInParamsFile;

    FraggerOutputType(String extension, String valueInParamsFile) {
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
