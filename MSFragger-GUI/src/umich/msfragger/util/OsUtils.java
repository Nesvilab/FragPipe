/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.util;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.CodeSource;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Dmitry Avtonomov
 */
public class OsUtils {
    
    public static enum ARCH {
        X86(32),
        AMD64(64),
        IA64(64);
        
        public final int bit;
        
        ARCH(int bit) {
            this.bit = bit;
        }
    }
    
    public static enum OS_FAMILY {
        WINDOWS,
        MAC,
        LINUX,
        FREEBSD
    }
    
    private OsUtils() {}
    
    public static boolean isWindows() {
        String osName = System.getProperty("os.name");
        if (osName == null)
            return true; // just the default
        return osName.toLowerCase().startsWith("win");
    }
    
    
    
    /**
     * OS name. E.g. 'Linux' or 'Windows XP'.
     * @return 
     */
    public static String getOsName() {
        String osName = System.getProperty("os.name");
        return osName;
    }
    
    
    /**
     * Tries to determine processor architecture from system properties.
     * @return  null if could not determine.
     */
    public static ARCH getSystemArch() {
        
        try {
            if (isWindows()) {
                String arch = System.getenv("PROCESSOR_ARCHITECTURE");
                String wow64 = System.getenv("PROCESSOR_ARCHITEW6432");

                if (wow64 != null)
                    return ARCH.valueOf(wow64.toUpperCase());
                else if (arch != null)
                    return ARCH.valueOf(arch.toUpperCase());
            } else {
                //String osName = System.getProperty("os.name");
                String osArch = System.getProperty("os.arch");
                return ARCH.valueOf(osArch.toUpperCase());
            }
        } catch (IllegalArgumentException e) {
            // could not map os.arch or whatever to our enum values, not biggie
        }
        
        return null;
    }
}
