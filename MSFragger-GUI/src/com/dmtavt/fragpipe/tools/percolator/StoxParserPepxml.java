package com.dmtavt.fragpipe.tools.percolator;

import org.codehaus.stax2.XMLInputFactory2;
import org.codehaus.stax2.XMLStreamReader2;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.stream.XMLStreamException;
import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class StoxParserPepxml {
    private static final Logger log = LoggerFactory.getLogger(StoxParserPepxml.class);

    @FunctionalInterface
    interface XmlConditionCallback {
        boolean accept(XMLStreamReader2 sr) throws XMLStreamException;
    }

    /**
     * Called for the beginning of a tag and its content.
     */
    interface TagPairCallback {
        void tagStart(String tagName, XMLStreamReader2 sr) throws XMLStreamException;

        void tagContents(String tagName, StringBuilder sb);
    }

    /**
     * Advance XMLStreamReader as long as the callback returns true.
     */
    public static void processUntilTrue(XMLStreamReader2 sr, XmlConditionCallback callback) throws XMLStreamException {
        do {
            if (callback.accept(sr))
                return;
        } while (sr.hasNext() && sr.next() >= 0);
        throw new IllegalStateException("xml document ended without callback returning true");
    }

    public static boolean isTagStart(XMLStreamReader2 sr, String tagName) {
        return XMLStreamReader2.START_ELEMENT == sr.getEventType() && tagName.equalsIgnoreCase(sr.getLocalName());
    }

    public static boolean isTagStart(XMLStreamReader2 sr) {
        return XMLStreamReader2.START_ELEMENT == sr.getEventType();
    }

    public static boolean isTagEnd(XMLStreamReader2 sr, String tagName) {
        return XMLStreamReader2.END_ELEMENT == sr.getEventType() && tagName.equalsIgnoreCase(sr.getLocalName());
    }

    /** Main parsing function. **/
    public static List<NameValue> parseSearchSummary(Path path) throws IOException, XMLStreamException {
        XMLInputFactory2 f = (XMLInputFactory2) XMLInputFactory2.newFactory();
        f.configureForSpeed();
//        f.configureForLowMemUsage();
        XMLStreamReader2 sr = null;
        try (BufferedReader br = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            sr = (XMLStreamReader2) f.createXMLStreamReader(br);

            final String tagSearchSummary = "search_summary";
            final String tagParameter = "parameter";
            // fast forward to beginning 'search_summary' tag (will throw if we don't find the tag at all)
            processUntilTrue(sr, sr1 -> isTagStart(sr1, tagSearchSummary));

            final List<NameValue> parameters = new ArrayList<>();
            final StringBuilder sb = new StringBuilder();

            // now keep processing unless we reach closing 'search_summary' tag
            processUntilTrue(sr, sr1 -> {
                if (isTagEnd(sr1, tagSearchSummary))
                    return true;

                if (isTagStart(sr1, tagParameter)) {
                    NameValue nv = new NameValue();
                    processTagPair(sr1, sb, nv);
                    processUntilTrue(sr1, sr2 -> {
                        if (isTagEnd(sr2, tagParameter)) {
                            return true;
                        }
                        sr2.next();
                        return false;
                    });
                    if (nv.isComplete()) {
                        parameters.add(nv);
                    } else {
                        throw new IllegalStateException("Whoa, a parameter had incomplete data");
                    }
                }

                return false;
            });
            return parameters;

        } finally {
            if (sr != null)
                sr.close();
        }

    }

    public static void processTagPair(XMLStreamReader2 sr, StringBuilder sb, TagPairCallback callback) throws XMLStreamException {
        final String tagName = sr.getLocalName();
        callback.tagStart(tagName, sr); // let the caller do whatever they need with the tag name and attributes
        sb.setLength(0); // clear our buffer, preparing to read the characters inside
        processUntilTrue(sr, sr1 -> {
            switch (sr1.getEventType()) {
                case XMLStreamReader2.END_ELEMENT: // ending condition
                    callback.tagContents(tagName, sb); // let the caller do whatever they need with text contents of the tag
                    return true;
                case XMLStreamReader2.CHARACTERS:
                    sb.append(sr1.getText());
                    break;
            }
            return false;
        });
    }

    public static class NameValue implements TagPairCallback {
        public String k = null;
        public String v = null;

        public NameValue(String k, String v) {
            this.k = k;
            this.v = v;
        }

        public NameValue() {
        }

        boolean isComplete() {
            return k != null && v != null;
        }

        @Override
        public void tagStart(String tagName, XMLStreamReader2 sr) throws XMLStreamException {
            switch (tagName) {
                case "parameter": {
                    final int idxName = sr.getAttributeIndex("", "name");
                    if (idxName < 0)
                        return;
                    this.k = sr.getAttributeValue(idxName);
                    this.v = sr.getAttributeValue(sr.getAttributeIndex("", "value"));
                    break;
                }
            }
        }

        @Override
        public void tagContents(String tagName, StringBuilder sb) {
            //log.warn("tagContents: tagName=[{}], sb=[{}]", tagName, sb.toString());
        }
    }
}
