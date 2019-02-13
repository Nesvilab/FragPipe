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
package umich.msfragger.util;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.CodeSource;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import javax.swing.filechooser.FileFilter;
import umich.msfragger.gui.MsfraggerGuiFrame;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PathUtils {

    /**
     * @return null if path does not exist or contains illegal characters. The actual normalized
     * path otherwise.
     */
    public static Path isExisting(String path) {
        try {
            Path p = Paths.get(path);
            if (Files.exists(p))
                return p;
        } catch (Exception ignored) {}
        return null;
    }

    public static String testFilePath(String fileName, String dir) {
        try {
            Path fileNameWasAbsolute = Paths.get(fileName);
            if (Files.exists(fileNameWasAbsolute)) {
                return fileNameWasAbsolute.toAbsolutePath().toString();
            }
        } catch (Exception e) {
            // something wrong with the path
        }
        try {
            Path fileNameWasRelative = Paths.get(dir, fileName);
            if (Files.exists(fileNameWasRelative)) {
                return fileNameWasRelative.toAbsolutePath().toString();
            }
        } catch (Exception e) {
            // something wrong with the path
        }
        return null;
    }
    
    /**
     * Searches for a file first in provided paths, then in working dir, then in system path.<br/>
     * If you want to search near the JAR file currently being executed, add
     * its path to the 'paths' parameter.
     * 
     * @param searchSystemPath
     * @param recursive
     * @see #getCurrentJarUri()
     * 
     * @param name  File name to search for.
     * @param paths
     * @return 
     */
    public static Path findFile(String name, boolean searchSystemPath, boolean recursive, String... paths) {
        
        List<String> searchPaths = new ArrayList<>(paths.length);
        for (String path : paths) {
            if (StringUtils.isNullOrWhitespace(path)) {
                searchPaths.add(path);
            }
        }
        if (searchSystemPath) {
            searchPaths.addAll(getSystemPaths());
        }
        
        // search the paths
        for (String path : searchPaths) {
            Path search = Paths.get(path, name);
            if (Files.exists(search)) {
                return search;
            }
        }
        
        if (recursive) {
            for (String path : searchPaths) {
                return findFile(name, false, true);
            }
        }
        
        return null;
    }
    
    /**
     * Searches for a file first in provided paths, then in system path.<br/>
     * If you want to search near the JAR file currently being executed, add
     * its path to the 'paths' parameter.
     * 
     * @param searchSystemPath
     * @param recursive
     * @see #getCurrentJarUri()
     * 
     * @param name  File name to search for.
     * @param paths
     * @return 
     */
    public static Path findFile(Pattern name, boolean searchSystemPath, boolean recursive, String... paths) {
        
        List<String> searchPaths = new ArrayList<>(paths.length);
        for (String path : paths) {
            if (!StringUtils.isNullOrWhitespace(path)) {
                searchPaths.add(path);
            }
        }
        if (searchSystemPath) {
            searchPaths.addAll(getSystemPaths());
        }
        
        // search the paths
        for (String path : searchPaths) {
            Path search = searchDirectory(name, Paths.get(path));
            if (search != null) {
                return search;
            }
        }
        
        if (recursive) {
            List<String> deeperSearchPaths = new ArrayList<>();
            for (String path : searchPaths) {
                Path p = Paths.get(path);
                if (Files.isDirectory(p)) {
                    
                    try {
                        Iterator<Path> dirIt = Files.newDirectoryStream(p).iterator();
                        while (dirIt.hasNext()) {
                            Path deeperFile = dirIt.next();
                            if (Files.isDirectory(deeperFile)) {
                                deeperSearchPaths.add(deeperFile.toString());
                            }
                        }
                    } catch (IOException ex) {
                        Logger.getLogger(PathUtils.class.getName()).log(Level.SEVERE, null, ex);
                    }
                    return findFile(name, false, true);
                }
            }
            if (!deeperSearchPaths.isEmpty()) {
                return findFile(name, false, true, deeperSearchPaths.toArray(new String[deeperSearchPaths.size()]));
            }
        }
        
        return null;
    }
    
    private static Path searchDirectory(Pattern name, Path p) {
        if (!Files.isDirectory(p))
            return null;
        try {
            Iterator<Path> dirIt = Files.newDirectoryStream(p).iterator();
            while(dirIt.hasNext()) {
                Path next = dirIt.next();
                if (!Files.isDirectory(next) && name.matcher(next.getFileName().toString()).matches()) {
                    return next;
                }
            }
        } catch (IOException ex) {
            Logger.getLogger(PathUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static List<String> getSystemPaths() {
        List<String> res = new ArrayList<>();
        String envPath = System.getenv("PATH");
        if (!StringUtils.isNullOrWhitespace(envPath)) {
            String[] split = envPath.split(File.pathSeparator);
            for (String s : split) {
                if (!StringUtils.isNullOrWhitespace(s)) {
                    res.add(s);
                }
            }
        }
        return res;
    }
    
    /**
     * Returns the value for the program, that will work with process builder.<br/>
     * Tries the supplied paths first, then tries to run without any path, i.e.
     * uses the system's PATH variable.
     * @param program  Name of the program to run, e.g. 'start.exe'.
     * @param paths  Additional paths to search in.
     * @return  Null if no working combo has been found.
     */
    public static String testBinaryPath(String program, String... paths) {
        
        // look in provided paths
        for (String path : paths) {
            if (StringUtils.isNullOrWhitespace(path))
                continue;
            try {
                List<String> commands = new LinkedList<>();
                String absPath = Paths.get(path, program).toAbsolutePath().toString();
                commands.add(absPath);
                ProcessBuilder pb = new ProcessBuilder(commands);
                Process proc = pb.start();
                proc.destroy();
                return absPath;
            } catch (Exception e1) {
                // could not run the program with absolute path
            }
        }
        
        // now final resort - try running just the program, hoping that it's in the PATH
        List<String> commands = new LinkedList<>();
        commands.add(program);
        ProcessBuilder pb = new ProcessBuilder(commands);
        try {
            Process proc = pb.start();
            proc.destroy();
            return program;
        } catch (Exception e1) {
            
        }
        return null;
    }

    public static void traverseDirectoriesAcceptingFiles(File dir, FileFilter filter, List<Path> accepted) {
        if (!dir.isDirectory()) {
            if (filter.accept(dir)) {
                accepted.add(Paths.get(dir.getAbsolutePath()));
            }
        }
        Path dirPath = Paths.get(dir.getAbsolutePath());
        try {
            DirectoryStream<Path> ds = Files.newDirectoryStream(dirPath);
            Iterator<Path> it = ds.iterator();
            while (it.hasNext()) {
                File next = it.next().toFile();
                boolean isDir = next.isDirectory();
                if (isDir) {
                    traverseDirectoriesAcceptingFiles(next, filter, accepted);
                } else {
                    if (filter.accept(next)) {
                        accepted.add(Paths.get(next.getAbsolutePath()));
                    }
                }
            }
        } catch (IOException ex) {
            Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public static URI getCurrentJarUri() {
        try {
            CodeSource codeSource = OsUtils.class.getProtectionDomain().getCodeSource();
            URL location = codeSource.getLocation();
            return location.toURI();
        } catch (URISyntaxException ex) {
            Logger.getLogger(OsUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    private PathUtils() {}
}
