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

/**
 *
 * @author Dmitry Avtonomov
 */
public class Mod {
    
    public final double massDelta;
    public final String sites;
    public final boolean isEnabled;

    public Mod(double massDelta, String sites, boolean isEnabled) {
        this.massDelta = massDelta;
        this.sites = sites;
        this.isEnabled = isEnabled;
    }
    
}
