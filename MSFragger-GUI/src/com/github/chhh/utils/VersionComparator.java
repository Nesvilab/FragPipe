package com.github.chhh.utils;

import com.github.chhh.utils.VersionComparator.VersionTokenizer.TOKEN_TYPE;
import com.github.chhh.utils.VersionComparator.VersionTokenizer.Token;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
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
        int p1 = 0;
        Token t1prev = null;
        int p2 = 0;
        Token t2prev = null;
        while(p1 < ts1.size() || p2 < ts2.size()) {
            Token t1 = ts1.get(p1);
            Token t2 = ts1.get(p2);
            if (t1.type == t2.type) {

            } else {
                // Different types of tokens.
                // Previous token might have been a number though for cases like comparing 0.1 and 0.1.2
                if (t1.type == TOKEN_TYPE.NUM)
                return t1.val.compareTo(t2.val);
            }
        }
    }

//    @Override
//    public int compare(String o1, String o2) {
//
//        VersionTokenizer tokenizer1 = new VersionTokenizer(o1);
//        VersionTokenizer tokenizer2 = new VersionTokenizer(o2);
//
//        int number1, number2;
//        String suffix1, suffix2;
//
//        while (tokenizer1.MoveNext()) {
//            if (!tokenizer2.MoveNext()) {
//                do {
//                    number1 = tokenizer1.getNumber();
//                    suffix1 = tokenizer1.getSuffix();
//                    if (number1 != 0 || suffix1.length() != 0) {
//                        // Version one is longer than number two, and non-zero
//                        return 1;
//                    }
//                } while (tokenizer1.MoveNext());
//
//                // Version one is longer than version two, but zero
//                return 0;
//            }
//
//            number1 = tokenizer1.getNumber();
//            suffix1 = tokenizer1.getSuffix();
//            number2 = tokenizer2.getNumber();
//            suffix2 = tokenizer2.getSuffix();
//
//            if (number1 < number2) {
//                // Number one is less than number two
//                return -1;
//            }
//            if (number1 > number2) {
//                // Number one is greater than number two
//                return 1;
//            }
//
//            boolean empty1 = suffix1.length() == 0;
//            boolean empty2 = suffix2.length() == 0;
//
//            if (empty1 && empty2) {
//                continue; // No suffixes
//            }
//            if (empty1) {
//                return 1; // First suffix is empty (1.2 > 1.2b)
//            }
//            if (empty2) {
//                return -1; // Second suffix is empty (1.2a < 1.2)
//            }
//            // Lexical comparison of suffixes
//            int result = suffix1.compareTo(suffix2);
//            if (result != 0) {
//                return result;
//            }
//
//        }
//        if (tokenizer2.MoveNext()) {
//            do {
//                number2 = tokenizer2.getNumber();
//                suffix2 = tokenizer2.getSuffix();
//                if (number2 != 0 || suffix2.length() != 0) {
//                    // Version one is longer than version two, and non-zero
//                    return -1;
//                }
//            } while (tokenizer2.MoveNext());
//
//            // Version two is longer than version one, but zero
//            return 0;
//        }
//        return 0;
//    }
//
//    private static class VersionTokenizer {
//
//        private final String _versionString;
//        private final int _length;
//
//        private int _position;
//        private int _number;
//        private String _suffix;
//
//        public int getNumber() {
//            return _number;
//        }
//
//        public String getSuffix() {
//            return _suffix;
//        }
//
//        VersionTokenizer(String versionString) {
//            if (versionString == null) {
//                throw new IllegalArgumentException("versionString is null");
//            }
//
//            _versionString = versionString;
//            _length = versionString.length();
//        }
//
//        boolean MoveNext() {
//            _number = 0;
//            _suffix = "";
//
//            // No more characters
//            if (_position >= _length) {
//                return false;
//            }
//
//            while (_position < _length) {
//                char c = _versionString.charAt(_position);
//                if (c < '0' || c > '9') {
//                    break;
//                }
//                _number = _number * 10 + (c - '0');
//                _position++;
//            }
//
//            int suffixStart = _position;
//
//            while (_position < _length) {
//                char c = _versionString.charAt(_position);
//                if (c == '.') {
//                    break;
//                }
//                _position++;
//            }
//
//            _suffix = _versionString.substring(suffixStart, _position);
//
//            if (_position < _length) {
//                _position++;
//            }
//
//            return true;
//        }
//    }
}

