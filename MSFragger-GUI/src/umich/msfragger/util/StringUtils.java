/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
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
        return s == null || WHITESPACE_STRING.matcher(s).matches();
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
