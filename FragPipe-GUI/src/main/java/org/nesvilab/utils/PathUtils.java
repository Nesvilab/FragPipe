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

import org.nesvilab.fragpipe.exceptions.ValidationException;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PathUtils {
    private static final org.slf4j.Logger log = LoggerFactory.getLogger(PathUtils.class);

    public static Stream<Path> findFilesQuietly(Path startDir, Predicate<Path> toAccept) {
        try {
            return Files.walk(startDir).filter(toAccept);
        } catch (IOException e) {
            log.error("Error traversing files", e);
            return Stream.empty();
        }
    }

    public static List<String> getClasspaths() {
        String nameCp = "java.class.path";
        String nameSep = "path.separator";

        String classpath = System.getProperty(nameCp);
        String sep = System.getProperty(nameSep);
        return Arrays.asList(classpath.split(sep));
    }

    /** Silently creates the whole directory structure.
     * Only throws if given path exists and is not a directory. */
    public static Path createDirs(Path dir) throws IOException {
        if (Files.exists(dir)) {
            if (Files.isDirectory(dir)) {
                return dir;
            }
            throw new IOException("Exists and not a directory: " + dir.toString());
        }
        // not exists
        return Files.createDirectories(dir);
    }

    /**
     * Returns the set of unique directories containing stuff that's on classpath.
     */
    public static Set<String> getClasspathDirs() {
        List<String> cps = PathUtils.getClasspaths();
        LinkedHashSet<String> classpaths = new LinkedHashSet<>();
        for (String cp : cps) {
            Path path;
            try {
                path = Paths.get(cp);
            } catch (InvalidPathException e) {
                continue;
            }

            boolean exists;
            try {
                exists = Files.exists(path);
            } catch (Exception e) {
                continue;
            }
            if (!exists) {
                continue;
            }

            boolean isDir;
            try {
                isDir = Files.isDirectory(path);
            } catch (Exception e) {
                continue;
            }
            if (isDir) {
                classpaths.add(path.toString());
            } else {
                Path parent = path.toAbsolutePath().getParent();
                classpaths.add(parent != null ? parent.toString() : ".");
            }
        }

        return classpaths;
    }

    /**
     * Existing Path or null if path is invalid or does not exist. All exceptions are swallowed.
     * @return null if path does not exist or contains illegal characters. The actual path otherwise.
     */
    public static Path existing(String path) {
        if (StringUtils.isBlank(path))
            return null;
        try {
            Path p = Paths.get(path);
            if (Files.exists(p))
                return p;
        } catch (Exception ignored) {
        }
        return null;
    }

    public static Path existing(Path path) {
        return existing(path.toString());
    }

    /**
     * Existing Path or null if path is invalid or does not exist.
     * @param doThrow If false will swallow exceptions instead of throwing.
     * @return null if path does not exist or contains illegal characters. The actual path otherwise.
     * @throws ValidationException in case anything is wrong with the path or it does not exist. And
     * only if 'doThrow' is true.
     */
    public static Path existing(String path, boolean doThrow) throws ValidationException {
        if (StringUtils.isBlank(path)) {
            if (doThrow)
                throw new ValidationException("Empty or null path");
            else
                return null;
        }
        Path p;
        try {
            p = Paths.get(path);
        } catch (InvalidPathException e) {
            if (doThrow)
                throw new ValidationException("Not a valid path: " + path, e);
            else
                return null;
        }
        if (Files.notExists(p)) {
            if (doThrow)
                throw new ValidationException("File does not exist: " + path);
            else
                return null;
        } else {
            return p;
        }
    }

    public static String testFilePath(String fileName, String dir) {
        try {
            Path fileNameWasAbsolute = Paths.get(fileName);
            if (Files.exists(fileNameWasAbsolute)) {
                return fileNameWasAbsolute.toAbsolutePath().normalize().toString();
            }
        } catch (Exception e) {
            // something wrong with the path
        }
        try {
            Path fileNameWasRelative = Paths.get(dir, fileName);
            if (Files.exists(fileNameWasRelative)) {
                return fileNameWasRelative.toAbsolutePath().normalize().toString();
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
     * @see JarUtils#getCurrentJarUri()
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
     * @see JarUtils#getCurrentJarUri()
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
                String absPath = Paths.get(path, program).toAbsolutePath().normalize().toString();
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

    public static void traverseDirectoriesAcceptingFiles(File start, Predicate<File> predicate, List<Path> accepted, boolean digAfterAccepting) {

        if (!start.isDirectory()) {
            // start file is not a dir
            if (predicate.test(start)) {
                accepted.add(Paths.get(start.getAbsolutePath()));
            }
        } else {
            // start file is a dir
            if (predicate.test(start)) {
                accepted.add(start.toPath());
                if (!digAfterAccepting) {
                    return;
                }
            }
            try {
                List<Path> content = Files.list(start.toPath()).collect(Collectors.toList());
                for (Path path : content) {
                    traverseDirectoriesAcceptingFiles(path.toFile(), predicate, accepted, digAfterAccepting);
                }
            } catch (IOException e) {
                log.error("Error traversing directories", e);
            }
        }
    }
}
