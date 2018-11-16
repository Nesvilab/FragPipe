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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author Dmitry Avtonomov
 */
public class StringUtils {
    private static Pattern WHITESPACE_STRING = Pattern.compile("^\\s*$");

    public static String upToLastDot(String s) {
        int last = s.lastIndexOf('.');
        return last < 0 ? s : s.substring(0, last);
    }
    
    public static String afterLastDot(String s) {
        int last = s.lastIndexOf('.');
        return last < 0 ? "" : s.substring(last+1);
    }

    public static String upToLastChar(String s, char ch, boolean emptyIfNoChar) {
        int last = s.lastIndexOf(ch);
        if (last < 0)
            return emptyIfNoChar ? "" : s;
        return s.substring(0, last);
    }

    public static String afterLastChar(String s, char ch, boolean emptyIfNoChar) {
        int last = s.lastIndexOf(ch);
        if (last < 0)
            return emptyIfNoChar ? "" : s;
        return s.substring(last+1);
    }
    
    public static List<String> splitTrim(String input, String sep) {
        String[] split = input.split(sep);
        List<String> strings = new ArrayList<>(split.length);
        for (int i = 0; i < split.length; i++) {
            split[i] = split[i].trim();
            if (!split[i].isEmpty()) {
                strings.add(split[i]);
            }
        }
        return strings;
    }
    private StringUtils() {}
    
    
    public static boolean isNullOrWhitespace(String s) {
        return s == null || s.length() == 0 || WHITESPACE_STRING.matcher(s).matches();
    }
    
    /**
     * Will trim all whitespace and return the non-zero length strings that are left.
     * @param regex e.g. "\\s+" to split on any number of whitespaces
     * @param line to be split
     * @return empty array if there was nothing but whitespace
     */
    public static List<String> split(String regex, String line) {
        line = line.trim();
        if (line.isEmpty())
            return Collections.emptyList();
        String[] split = line.split(regex);
        LinkedList<String> list = new LinkedList<>();
        for (String s : split) {
            s = s.trim();
            if (!s.isEmpty())
                list.add(s);
        }
        return list;
    }
    
    public static List<String> splitCommandLine(String line) {
        String pattern = "(\"[^\"]+\"|[^\\s\"]+)";
        Pattern regex = Pattern.compile(pattern);
        Matcher matcher = regex.matcher(line);
        LinkedList<String> list = new LinkedList<>();
        while (matcher.find()) {
            list.add(matcher.group(1));
        }
        return list;
    }
    
    public static String join(String[] strings, String sep) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < strings.length; i++) {
            String string = strings[i];
            sb.append(string);
            if (i < strings.length - 1)
                sb.append(sep);
        }
        return sb.toString();
    }
}
