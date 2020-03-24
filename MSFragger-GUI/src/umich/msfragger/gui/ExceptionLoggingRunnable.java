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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.chhh.utils.LogUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class ExceptionLoggingRunnable implements Runnable {
    private static final Logger log = LoggerFactory.getLogger(ExceptionLoggingRunnable.class);
    Runnable delegate;
    Appendable[] outs;

    public ExceptionLoggingRunnable(Runnable delegate, Appendable... out) {
        this.delegate = delegate;
        this.outs = out;
    }

    @Override
    public void run() {
        try {
            delegate.run();
        } catch (Exception e) {
            String msg = String.format(
                "Something bad happened in a worker thread:\n%s\n"
                + "~~~~~~~~~~~~~~~~~~~~~~~~\n"
                + "Some details: ", e.getMessage(), LogUtils.stacktrace(e));
            log.error(msg, e);
            for (Appendable out : outs) {
                LogUtils.println(out, msg);
            }
        }
    }
    
}
