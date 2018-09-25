/*
 * Copyright 2018 Dmitry Avtonomov.
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
package umich.msfragger.util;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PythonInfo {
    public final String cmd;
    public final boolean isPython3;
    public final boolean isNumpy;
    public final boolean isPandas;
    
    public PythonInfo(String cmd, boolean isPython3, boolean isNumpy, boolean isPandas) {
        this.cmd = cmd;
        this.isPython3 = isPython3;
        this.isNumpy = isNumpy;
        this.isPandas = isPandas;
    }
}
