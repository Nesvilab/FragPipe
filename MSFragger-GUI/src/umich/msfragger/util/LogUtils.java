package umich.msfragger.util;

import java.awt.Color;
import javax.swing.*;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.LogManager;
import umich.swing.console.TextConsole;

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
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    out.append(toPrint);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        };


        if (doOnEDT) {
            SwingUtilities.invokeLater(runnable);
        } else {
            runnable.run();
        }
    }

    public static final void println(Appendable out, String toPrint) {
        println(out, toPrint, true);
    }
    
    public static final void println(final Appendable out, final String toPrint, boolean doOnEDT) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    out.append(toPrint);
                    out.append("\n");
                } catch (IOException e) {
                    e.printStackTrace();
                }
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
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    out.append(c, toPrint);
                    out.append("\n");
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        };


        if (doOnEDT) {
            SwingUtilities.invokeLater(runnable);
        } else {
            runnable.run();
        }
    }
}
