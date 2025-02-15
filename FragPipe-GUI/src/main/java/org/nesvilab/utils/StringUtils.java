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

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * String manipulation utilities.
 *
 * @author Dmitry Avtonomov
 */
public final class StringUtils {
  private static Pattern RE_WHITESPACE = Pattern.compile("^\\s*$");

  private StringUtils() {
    throw new AssertionError("This class can not be instantiated");
  }

  /** Removes prefix if present. */
  public static String stripLeading(String text, String prefix) {
    if (text.startsWith(prefix))
      return text.substring(prefix.length());
    return text;
  }

  public static String sortedChars(String s) {
    char[] chars = s.toCharArray();
    Arrays.sort(chars);
    return new String(chars);
  }

  public static boolean isPureAscii(String text) {
    return StandardCharsets.US_ASCII.newEncoder().canEncode(text);
  }

  public static List<String> splitCommandLine(String line) {
    String pattern = "([\"'][^\"']+[\"']|[^\\s\"']+)";
    Pattern regex = Pattern.compile(pattern);
    Matcher matcher = regex.matcher(line);
    LinkedList<String> list = new LinkedList<>();
    while (matcher.find()) {
      list.add(matcher.group(1));
    }
    return list;
  }

  /**
   * @return True if any: 1) string is null, 2) string.length == 0, 3) string is made of
   * any number of whitespace characters.
   */
  public static boolean isNullOrWhitespace(String s) {
    return s == null || s.length() == 0 || RE_WHITESPACE.matcher(s).matches();
  }

  /**
   * Prepends prefix only if the base doesn't yet start with this prefix.
   */
  public static String prependOnce(String base, String prefix) {
    if (base == null) return prefix;
    if (prefix == null) return base;
    return base.startsWith(prefix) ? base : prefix + base;
  }

  /**
   * Appends suffix only if the base doesn't yet end with this suffix.
   */
  public static String appendOnce(String base, String suffix) {
    if (base == null) return suffix;
    if (suffix == null) return base;
    return base.endsWith(suffix) ? base : base + suffix;
  }

  public static String appendPrependOnce(String base, String quotes) {
    return prependOnce(appendOnce(base, quotes), quotes);
  }

  /**
   * Shorthand for {@link #isNullOrWhitespace(String)}.
   */
  public static boolean isBlank(String s) {
    return isNullOrWhitespace(s);
  }

  /**
   * Shorthand for NOT {@link #isNullOrWhitespace(String)}.
   */
  public static boolean isNotBlank(String s) {
    return !isNullOrWhitespace(s);
  }

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

  public static String upToLastSubstr(String s, String substr, boolean emptyIfNoSubstr) {
    int last = s.lastIndexOf(substr);
    if (last < 0)
      return emptyIfNoSubstr ? "" : s;
    return s.substring(0, last);
  }

  public static String afterLastChar(String s, char ch, boolean emptyIfNoChar) {
    int last = s.lastIndexOf(ch);
    if (last < 0)
      return emptyIfNoChar ? "" : s;
    return s.substring(last+1);
  }

  /**
   * Join strings with a separator.
   */
  public static String join(final Iterable<?> iterable, final String separator) {
    if (iterable == null) {
      return null;
    }
    return join(iterable.iterator(), separator);
  }

  /**
   * Join strings with a separator.
   */
  public static String join(final Iterator<?> iterator, final String separator) {

    if (iterator == null) {
      return null;
    }
    if (!iterator.hasNext()) {
      return "";
    }
    final Object first = iterator.next();
    if (!iterator.hasNext()) {
      final String result = Objects.toString(first);
      return result;
    }

    // two or more elements
    final StringBuilder buf = new StringBuilder(256);
    if (first != null) {
      buf.append(first);
    }

    while (iterator.hasNext()) {
      if (separator != null) {
        buf.append(separator);
      }
      final Object obj = iterator.next();
      if (obj != null) {
        buf.append(obj);
      }
    }
    return buf.toString();
  }

  /**
   * A common method for all enums since they can't have another base class
   *
   * @param <T> Enum type
   * @param c enum type. All enums must be all caps.
   * @param string case insensitive
   * @return corresponding enum, or null
   */
  public static <T extends Enum<T>> T getEnumFromString(Class<T> c, String string) {
    if (c != null && string != null) {
      return Enum.valueOf(c, string.trim().toUpperCase());
    }
    return null;
  }

  /**
   * Wrap text to some length.
   *
   * @param text the text to be wrapped
   * @param indent indentation size after a line has wrapped
   * @param colSize the maximum width of a line
   * @param out where to append the output.
   */
  public static void wrap(String text, int indent, int colSize, StringBuilder out) {
    String[] words = text.split("[ \\t]");
    int i = 0;
    int caretPos = 0;
    while (i < words.length) {
      String word = words[i];
      caretPos += word.length();
      if (caretPos <= colSize) {
        out.append(word);
        if (caretPos != colSize && !word.endsWith("\n")) {
          out.append(" ");
        }
        i++;
      } else {
        caretPos = 0;
        out.append("\n").append(repeat(" ", indent));
        caretPos += indent;
      }
    }
  }

  /**
   * Repeat a given string N times.
   *
   * @param str the string to be repeated
   * @param i number of times to repeat
   * @return a new string
   */
  public static String repeat(String str, int i) {
    return i > 0 ? new String(new char[i]).replace("\0", str) : "";
  }

  public static boolean isBlank(final CharSequence cs) {
    int strLen;
    if (cs == null || (strLen = cs.length()) == 0) {
      return true;
    }
    for (int i = 0; i < strLen; i++) {
      if (!Character.isWhitespace(cs.charAt(i))) {
        return false;
      }
    }
    return true;
  }
}

