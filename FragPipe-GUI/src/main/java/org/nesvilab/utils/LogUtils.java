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
package org.nesvilab.utils;

import java.awt.Color;
import javax.swing.SwingUtilities;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.LogManager;
import org.nesvilab.utils.swing.TextConsole;
import org.slf4j.LoggerFactory;

/**
 * Created by Dmitry Avtonomov on 2016-04-28.
 */
public class LogUtils {
    private LogUtils(){}

    /**
     * Configures JUL (java.util.logging) using the logging.properties file located in this
     * package. Only use this method for testing purposes, clients should configure
     * logging themselves - that is you need to provide a logging bridge for SLF4J
     * compatible to your logging infrastructure, or use SLF4J no-op logger.
     *
     * @param is input stream from which to read config. Normally will be obtained like
     *           {@code SomeClass.class.getResourceAsStream("logging.properties"}, an example
     *           of logging.properties file can be found in your JRE (e.g. /jdk1.7.0_80/jre/lib/logging.properties)
     */
    public static final void configureJavaUtilLogging(InputStream is) {
        try {
            LogManager logMan = LogManager.getLogManager();
            logMan.readConfiguration(new BufferedInputStream(is));
        } catch (final IOException e) {
            java.util.logging.Logger.getAnonymousLogger().severe(
                    "Could not load development logging.properties file using "
                            + "LogHelper.class.getResourceAsStream(\"/logging.properties\")");
            java.util.logging.Logger.getAnonymousLogger().severe(e.getMessage());
        }
    }

    public static final void print(Appendable out, String toPrint) {
        print(out, toPrint, true);
    }

    public static final void print(final Appendable out, final String toPrint, boolean doOnEDT) {
        Runnable runnable = () -> {
            try {
                out.append(toPrint);
            } catch (IOException e) {
                e.printStackTrace();
            }
        };


        if (doOnEDT) {
            SwingUtilities.invokeLater(runnable);
        } else {
            runnable.run();
        }
    }

    public static void logMethodStart(Object clazzInstance, String methodName) {
        logMethodStart(clazzInstance.getClass(), methodName);
    }

    public static void logMethodStart(Class<?> clazz, String methodName) {
        LoggerFactory.getLogger(clazz).debug("Start {}", methodName);
    }

    public static void logMethodDone(Object clazzInstance, String methodName) {
        logMethodDone(clazzInstance.getClass(), methodName);
    }

    public static void logMethodDone(Class<?> clazz, String methodName) {
        LoggerFactory.getLogger(clazz).debug("Done {}", methodName);
    }

    public static final void println(Appendable out, String toPrint) {
        println(out, toPrint, true);
    }
    
    public static final void println(final Appendable out, final String toPrint, boolean doOnEDT) {
        Runnable runnable = () -> {
            try {
                out.append(toPrint);
                out.append("\n");
            } catch (IOException e) {
                e.printStackTrace();
            }
        };


        if (doOnEDT) {
            SwingUtilities.invokeLater(runnable);
        } else {
            runnable.run();
        }
    }
    
    public static final void print(final Color c, final TextConsole out, boolean doOnEDT,
            final String toPrint, final boolean appendNewLine) {
        Runnable runnable = () -> {
            out.append(c, toPrint);
            if (appendNewLine) {
                out.append("\n");
            }
        };


        if (doOnEDT) {
            SwingUtilities.invokeLater(runnable);
        } else {
            runnable.run();
        }
    }

    public static final void printWithAnsiColorCodes(final TextConsole out, boolean doOnEDT,
        final String toPrint, final boolean appendNewLine) {
        Runnable runnable = () -> {
            out.append(toPrint);
            if (appendNewLine) {
                out.append("\n");
            }
        };


        if (doOnEDT) {
            SwingUtilities.invokeLater(runnable);
        } else {
            runnable.run();
        }
    }
}
