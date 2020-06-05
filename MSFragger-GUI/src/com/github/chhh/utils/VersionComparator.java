package com.github.chhh.utils;

import com.github.chhh.utils.VersionComparator.VersionTokenizer.TOKEN_TYPE;
import com.github.chhh.utils.VersionComparator.VersionTokenizer.Token;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Compares version number strings.
 */
public class VersionComparator implements Comparator<String> {
    private static VersionComparator INSTANCE = new VersionComparator();

    public static int cmp(String v1, String v2) {
        return INSTANCE.compare(v1, v2);
    }

    public boolean equals(String o1, String o2) {
        return compare(o1, o2) == 0;
    }

    public static class VersionTokenizer {

        private static Pattern[] re = {
            Pattern.compile("^([a-zA-Z]+)"),
            Pattern.compile("^([0-9]+)"),
            Pattern.compile("^([-_.]+)")};
        private static TOKEN_TYPE[] types = {TOKEN_TYPE.TXT, TOKEN_TYPE.NUM, TOKEN_TYPE.SEP};
        private VersionTokenizer() {}
        public enum TOKEN_TYPE {SEP, NUM, TXT}
        public static class Token {
            public final String val;
            public final TOKEN_TYPE type;
            public Token(String val, TOKEN_TYPE type) {
                this.val = val;
                this.type = type;
            }
        }

        public static List<Token> tokenize(String ver, boolean reportSeparators) {
            ArrayList<Token> tokens = new ArrayList<>();
            do {
                boolean found = false;
                for (int i = 0; i < re.length; i++) {
                    Matcher m = re[i].matcher(ver);
                    if (m.find()) {
                        Token token = new Token(m.group(1), types[i]);
                        ver = ver.substring(token.val.length());
                        if (reportSeparators || TOKEN_TYPE.SEP != types[i]) {
                            tokens.add(token);
                        }
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    throw new IllegalArgumentException("ver contained symbols other than [a-zA-Z0-9-_.]");
                }
            } while (ver.length() > 0);
            return tokens;
        }
    }

    @Override
    public int compare(String o1, String o2) {
        if (StringUtils.isBlank(o1) || StringUtils.isBlank(o2))
            throw new IllegalArgumentException("Empty/null strings not allowed as versions");
        String s1 = o1;
        String s2 = o2;
        List<Token> ts1 = VersionTokenizer.tokenize(s1, false);
        List<Token> ts2 = VersionTokenizer.tokenize(s2, false);
        if (ts1.isEmpty() || ts2.isEmpty())
            throw new IllegalStateException("Empty token list");

        Function<List<Token>, List<Tuple2<TOKEN_TYPE, List<String>>>> f = tokens -> {
            ArrayList<Tuple2<TOKEN_TYPE, List<String>>> l = new ArrayList<>();
            for (Token t : tokens) {
                if (!l.isEmpty() && l.get(l.size()-1).item1 == t.type) {
                    l.get(l.size()-1).item2.add(t.val);
                } else {
                    l.add(new Tuple2<>(t.type, new ArrayList<>(Collections.singletonList(t.val))));
                }
            }
            return l;
        };
        List<Tuple2<TOKEN_TYPE, List<String>>> g1 = f.apply(ts1);
        List<Tuple2<TOKEN_TYPE, List<String>>> g2 = f.apply(ts2);
        int res = 0;

        for (int i = 0; i < Math.min(g1.size(), g2.size()); i++) {
            final Tuple2<TOKEN_TYPE, List<String>> g1i = g1.get(i);
            final Tuple2<TOKEN_TYPE, List<String>> g2i = g2.get(i);

            if (g1i.item1 == TOKEN_TYPE.NUM && g2i.item1 == TOKEN_TYPE.NUM) {
                final Comparator<String> cmp = Comparator.comparingInt(Integer::parseInt);
                for (int j = 0; j < Math.max(g1i.item2.size(), g2i.item2.size()); j++) {
                    String v1 = g1i.item2.get(Math.min(g1i.item2.size()-1, j));
                    String v2 = g2i.item2.get(Math.min(g2i.item2.size()-1, j));
                    res = cmp.compare(v1, v2);
                    if (res != 0) {
                        return res;
                    }
                }
            } else {
                final Comparator<String> cmp = String::compareTo;
                for (int j = 0; j < Math.min(g1i.item2.size(), g2i.item2.size()); j++) {
                    String v1 = g1i.item2.get(j);
                    String v2 = g2i.item2.get(j);
                    res = cmp.compare(v1, v2);
                    if (res != 0) {
                        return res;
                    }
                }
                if (g1i.item2.size() != g2i.item2.size()) {
                    return g1i.item2.size() < g2i.item2.size() ? 1 : -1;
                }
            }
        }
        if (g1.size() != g2.size()) {
            return g1.size() < g2.size() ? 1 : -1;
        }

        return res;
    }

}

