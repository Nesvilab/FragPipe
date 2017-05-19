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
package umich.msfragger.util;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author Dmitry Avtonomov
 */
public class FileListing {
    protected Path path;
    boolean includeDirectories = false;
    boolean includeFiles = true;
    boolean followLinks = false;
    boolean recursive = true;
    Pattern pattern;
    private int MAX_RECURSION_DEPTH = 128; // helps against stupid symlinks leading to higher levels in file-tree

    public boolean isFollowLinks() {
        return followLinks;
    }

    public void setFollowLinks(boolean followLinks) {
        this.followLinks = followLinks;
    }

    public boolean isIncludeFiles() {
        return includeFiles;
    }

    public void setIncludeFiles(boolean includeFiles) {
        this.includeFiles = includeFiles;
    }

    public Path getPath() {
        return path;
    }

    public void setPath(Path path) {
        this.path = path.toAbsolutePath();
    }

    public boolean isIncludeDirectories() {
        return includeDirectories;
    }

    public void setIncludeDirectories(boolean includeDirectories) {
        this.includeDirectories = includeDirectories;
    }

    public boolean isRecursive() {
        return recursive;
    }

    public void setRecursive(boolean recursive) {
        this.recursive = recursive;
    }

    public Pattern getPattern() {
        return pattern;
    }

    public void setPattern(Pattern pattern) {
        this.pattern = pattern;
    }

    /**
     * @param path Starting path and the pattern for matching.
     * @param regex The regular expression that paths will be matched against.
     */
    public FileListing(Path path, String regex) {
        this.path = path.toAbsolutePath();
        this.pattern = Pattern.compile(regex);
    }

    /**
     * @param path Starting path and the pattern for matching.
     * @param regex The regular expression that paths will be matched against.
     */
    public FileListing(Path path, Pattern regex) {
        this.path = path.toAbsolutePath();
        this.pattern = regex;
    }

    /**
     *
     * @return
     */
    public List<Path> findFiles() {
        LinkOption[] options;
        if (followLinks) {
            options = new LinkOption[0];
        } else {
            options = new LinkOption[]{LinkOption.NOFOLLOW_LINKS};
        }
        if (includeFiles && !Files.isDirectory(path, options)) {
            // if a single file was given, just check it against the pattern
            if (matches(path)) {
                return Collections.singletonList(path);
            }
        } else {
            // a directory was given, let's search it
            ArrayList<Path> result = new ArrayList<>();
            if (includeDirectories && matches(path)) {
                // check the top-level directory
                result.add(path);
            }
            findFiles(path, result, options, 0);
            return result;
        }
        return Collections.emptyList();
    }

    private void findFiles(Path path, List<Path> matching, LinkOption[] options, int recursionLevel) {
        if (recursionLevel > MAX_RECURSION_DEPTH)
            return; // safety net
        try {
            DirectoryStream<Path> paths = Files.newDirectoryStream(path);
            for (Path p : paths) {
                if (Files.isDirectory(p, options)) {
                    if (includeDirectories && matches(p))
                        matching.add(p);
                    if (isRecursive())
                        findFiles(p, matching, options, recursionLevel);
                } else {
                    if (includeFiles && matches(p))
                        matching.add(p);
                }
            }
        } catch (IOException e) {
            //log.error("Could not list files in directory '{}'", path.toString());
        }
    }

    private boolean matches(Path s) {
        Matcher matcher = pattern.matcher(s.toString());
        return matcher.matches();
    }
}
