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
package umich.msfragger.params.fragger;

import java.net.URI;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerProperties {
    public static final String DOWNLOAD_URL = "http://inventions.umich.edu/technologies/7143_msfrager-ultrafast-and-comprehensive-identification-of-peptides-from-tandem-mass-spectra";
    public static final URI DOWNLOAD_URI = URI.create(DOWNLOAD_URL);
    
    public static final String PROP_LATEST_VERSION = "msfragger.version.latest-known";
}
