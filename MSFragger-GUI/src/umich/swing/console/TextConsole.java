/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.swing.console;

import java.io.IOException;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;

/**
 *
 * @author dmitriya
 */
public class TextConsole extends JTextPane implements Appendable {

    public TextConsole() {
    }
    
    
    @Override
    public Appendable append(CharSequence csq) throws IOException {
        //append(csq.toString());
        StyledDocument doc = getStyledDocument();
        try {
            doc.insertString(doc.getLength(), csq.toString(), null);
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public Appendable append(CharSequence csq, int start, int end) throws IOException {
        //append(csq.subSequence(start, end).toString());
        StyledDocument doc = getStyledDocument();
        try {
            doc.insertString(doc.getLength(), csq.subSequence(start, end).toString(), null);
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public Appendable append(char c) throws IOException {
        //append(Character.toString(c));
        StyledDocument doc = getStyledDocument();
        try {
            doc.insertString(doc.getLength(), String.valueOf(c), null);
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
        return this;
    }
}