/*
 * Copyright 2017 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.params.enums;

/**
 *
 * @author Dmitry Avtonomov
 */
public enum FraggerOutputType {
    
    PEP_XML("pepXML", "pepXML"),
    TSV("tsv", "tsv");
    
    
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
}
