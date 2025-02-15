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
package org.nesvilab.utils.swing;

import java.awt.Color;
import java.awt.Font;
import java.io.IOException;
import java.util.concurrent.locks.ReentrantLock;
import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

/**
 *
 * @author dmitriya
 */
public class TextConsole extends JTextPane implements Appendable {

    public static final Color D_Black = Color.getHSBColor(0.000f, 0.000f, 0.000f);
    public static final Color D_Red = Color.getHSBColor(0.000f, 1.000f, 0.502f);
    public static final Color D_Blue = Color.getHSBColor(0.667f, 1.000f, 0.502f);
    public static final Color D_Magenta = Color.getHSBColor(0.833f, 1.000f, 0.502f);
    public static final Color D_Green = Color.getHSBColor(0.333f, 1.000f, 0.502f);
    public static final Color D_Yellow = Color.getHSBColor(0.167f, 1.000f, 0.502f);
    public static final Color D_Cyan = Color.getHSBColor(0.500f, 1.000f, 0.502f);
    public static final Color D_White = Color.getHSBColor(0.000f, 0.000f, 0.753f);
    public static final Color B_Black = Color.getHSBColor(0.000f, 0.000f, 0.502f);
    public static final Color B_Red = Color.getHSBColor(0.000f, 1.000f, 1.000f);
    public static final Color B_Blue = Color.getHSBColor(0.667f, 1.000f, 1.000f);
    public static final Color B_Magenta = Color.getHSBColor(0.833f, 1.000f, 1.000f);
    public static final Color B_Green = Color.getHSBColor(0.333f, 1.000f, 1.000f);
    public static final Color B_Yellow = Color.getHSBColor(0.167f, 1.000f, 1.000f);
    public static final Color B_Cyan = Color.getHSBColor(0.500f, 1.000f, 1.000f);
    public static final Color B_White = Color.getHSBColor(0.000f, 0.000f, 1.000f);
    
    static final Color cReset = Color.getHSBColor(0.000f, 0.000f, 0.000f);
    static Color colorCurrent = cReset;

    private final Object lock = new Object();
    private ReentrantLock reentrantLock = new ReentrantLock(true);
    
    protected String remaining = "";
    private boolean forceWordWrapInsteadOfScroll;
    
    public TextConsole(boolean forceWordWrapInsteadOfScroll) {

        this.forceWordWrapInsteadOfScroll = forceWordWrapInsteadOfScroll;
    }

    public TextConsole() {
        this(true);
    }

    /**
     * Returns true if a viewport should always force the width of this
     * <code>Scrollable</code> to match the width of the viewport.
     *
     * @return true if a viewport should force the Scrollables width to match its own, false otherwise
     */
    @Override
    public boolean getScrollableTracksViewportWidth() {
        return forceWordWrapInsteadOfScroll;
    }

    public void setScrollableTracksViewportWidth(boolean preferWordWrap) {
        forceWordWrapInsteadOfScroll = preferWordWrap;
    }

    /**
     * Returns the text contained in this <code>TextComponent</code> in terms of the content type of
     * this editor.  If an exception is thrown while attempting to retrieve the text,
     * <code>null</code> will be returned. This is implemented to call <code>JTextComponent.write</code>
     * with a <code>StringWriter</code>.
     *
     * @return the text
     * @see #setText
     */
    @Override
    public String getText() {
        synchronized (lock) {
            return super.getText();
        }
    }

    @Override
    public void setText(String t) {
        synchronized (lock) {
            super.setText(t);
        }
    }

    @Override
    public Appendable append(CharSequence csq) {
        //append(csq.toString()); // too simple
        
        // old non-colored implementation
//        StyledDocument doc = getStyledDocument();
//        try {
//            doc.insertString(doc.getLength(), csq.toString(), null);
//        } catch (BadLocationException e) {
//            e.printStackTrace();
//        }

        synchronized (lock) {
            appendANSI(csq.toString());
        }
        return this;
    }

    @Override
    public Appendable append(CharSequence csq, int start, int end) {
        //append(csq.subSequence(start, end).toString()); // too simple
        
        // old non-colored implementation
//        StyledDocument doc = getStyledDocument();
//        try {
//            doc.insertString(doc.getLength(), csq.subSequence(start, end).toString(), null);
//        } catch (BadLocationException e) {
//            e.printStackTrace();
//        }

        synchronized (lock) {
            appendANSI(csq.subSequence(start, end).toString());
        }
        return this;
    }

    @Override
    public Appendable append(char c) {
        synchronized (lock) {
            //append(Character.toString(c));
            StyledDocument doc = getStyledDocument();
            try {
                doc.insertString(doc.getLength(), String.valueOf(c), null);
            } catch (BadLocationException e) {
                e.printStackTrace();
            }
        }
        return this;
    }
    
    public void append(Color c, String s) {
        synchronized (lock) {
            if (c == null) {
                appendANSI(s);
                return;
            }
            StyleContext sc = StyleContext.getDefaultStyleContext();
            AttributeSet aset = sc
                .addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.Foreground, c);
            int len = getDocument().getLength(); // same value as getText().length();
            setCaretPosition(len);  // place caret at the end (with no selection)
            setCharacterAttributes(aset, false);
            /**
             In JTextPane, only "\n" is recognized as a newline, so replace "\r\n" with "\n"
             see {@link javax.swing.text.DefaultEditorKit} for information on newlines
             “But while the document is in memory, the "\n" character is used to define a newline, regardless of how the newline is defined when the document is on disk.”
             JTextPane doesn't print "\r", so replace it with "\n"
             */
            replaceSelection(s.replace("\r\n", "\n")
                .replace("\r", "\u200B\n")); // there is no selection, so inserts at caret
        }
    }
    
    public void appendANSI(String s) { // convert ANSI color codes first
        synchronized (lock) {
            int aPos = 0;   // current char position in addString
            int aIndex = 0; // index of next Escape sequence
            int mIndex = 0; // index of "m" terminating Escape sequence
            String tmpString = "";
            boolean stillSearching = true; // true until no more Escape sequences
            String addString = remaining + s;
            remaining = "";

            if (addString.length() > 0) {
                aIndex = addString.indexOf("\u001B"); // find first escape
                if (aIndex
                    == -1) { // no escape/color change in this string, so just send it with current color
                    append(colorCurrent, addString);
                    return;
                }
                // otherwise There is an escape character in the string, so we must process it

                if (aIndex > 0) { // Escape is not first char, so send text up to first escape
                    tmpString = addString.substring(0, aIndex);
                    append(colorCurrent, tmpString);
                    aPos = aIndex;
                }
                // aPos is now at the beginning of the first escape sequence

                stillSearching = true;
                while (stillSearching) {
                    mIndex = addString.indexOf("m", aPos); // find the end of the escape sequence
                    if (mIndex < 0) { // the buffer ends halfway through the ansi string!
                        remaining = addString.substring(aPos, addString.length());
                        stillSearching = false;
                        continue;
                    } else {
                        tmpString = addString.substring(aPos, mIndex + 1);
                        colorCurrent = getANSIColor(tmpString);
                    }
                    aPos = mIndex + 1;
                    // now we have the color, send text that is in that color (up to next escape)

                    aIndex = addString.indexOf("\u001B", aPos);

                    if (aIndex
                        == -1) { // if that was the last sequence of the input, send remaining text
                        tmpString = addString.substring(aPos, addString.length());
                        append(colorCurrent, tmpString);
                        stillSearching = false;
                        continue; // jump out of loop early, as the whole string has been sent now
                    }

                    // there is another escape sequence, so send part of the string and prepare for the next
                    tmpString = addString.substring(aPos, aIndex);
                    aPos = aIndex;
                    append(colorCurrent, tmpString);

                } // while there's text in the input buffer
            }
        }
    }
    
    public Color getANSIColor(String ANSIColor) {
        if (ANSIColor.equals("\u001B[30m")) {
            return D_Black;
        } else if (ANSIColor.equals("\u001B[31m")) {
            return D_Red;
        } else if (ANSIColor.equals("\u001B[32m")) {
            return D_Green;
        } else if (ANSIColor.equals("\u001B[33m")) {
            return D_Yellow;
        } else if (ANSIColor.equals("\u001B[34m")) {
            return D_Blue;
        } else if (ANSIColor.equals("\u001B[35m")) {
            return D_Magenta;
        } else if (ANSIColor.equals("\u001B[36m")) {
            return D_Cyan;
        } else if (ANSIColor.equals("\u001B[37m")) {
            return D_White;
        } else if (ANSIColor.equals("\u001B[0;30m")) {
            return D_Black;
        } else if (ANSIColor.equals("\u001B[0;31m")) {
            return D_Red;
        } else if (ANSIColor.equals("\u001B[0;32m")) {
            return D_Green;
        } else if (ANSIColor.equals("\u001B[0;33m")) {
            return D_Yellow;
        } else if (ANSIColor.equals("\u001B[0;34m")) {
            return D_Blue;
        } else if (ANSIColor.equals("\u001B[0;35m")) {
            return D_Magenta;
        } else if (ANSIColor.equals("\u001B[0;36m")) {
            return D_Cyan;
        } else if (ANSIColor.equals("\u001B[0;37m")) {
            return D_White;
        } else if (ANSIColor.equals("\u001B[1;30m")) {
            return B_Black;
        } else if (ANSIColor.equals("\u001B[1;31m")) {
            return B_Red;
        } else if (ANSIColor.equals("\u001B[1;32m")) {
            return B_Green;
        } else if (ANSIColor.equals("\u001B[1;33m")) {
            return B_Yellow;
        } else if (ANSIColor.equals("\u001B[1;34m")) {
            return B_Blue;
        } else if (ANSIColor.equals("\u001B[1;35m")) {
            return B_Magenta;
        } else if (ANSIColor.equals("\u001B[1;36m")) {
            return B_Cyan;
        } else if (ANSIColor.equals("\u001B[1;37m")) {
            return B_White;
        } else if (ANSIColor.equals("\u001B[0m")) {
            return cReset;
        } else {
            //return B_White;
            return D_Black; // return black instead of white by default
        }
    }
}
