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
