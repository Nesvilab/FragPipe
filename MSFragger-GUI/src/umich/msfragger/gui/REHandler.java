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
package umich.msfragger.gui;

import umich.msfragger.util.LogUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class REHandler implements Runnable {
    
    Runnable delegate;
    Appendable[] outs;

    public REHandler(Runnable delegate, Appendable... out) {
        this.delegate = delegate;
        this.outs = out;
    }

    @Override
    public void run() {
        try {
            delegate.run();
        } catch (Exception e) {
            //log.error("Something bad happened in a worker thread", e);
            String msg = String.format("Something bad happened in a worker thread:\n%s", e.getMessage());
            for (Appendable out : outs) {
                LogUtils.println(out, msg);
            }
        }
    }
    
}
