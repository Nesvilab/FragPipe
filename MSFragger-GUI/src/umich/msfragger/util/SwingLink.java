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

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.util.Locale;

import javax.swing.JLabel;
import javax.swing.JOptionPane;

/**
 * A JLabel that behaves like a hyperlink, launching the default browser when
 * clicked.
 *
 * The link is styled like a standard &lt;a&gt; tag in a browser; blue and
 * underlined by default, and changing color as the user interacts with it.
 *
 * http://stackoverflow.com/q/527719/113632
 * https://bitbucket.org/dimo414/jgrep/src/tip/src/grep/SwingLink.java?fileviewer=file-view-default
 */
public class SwingLink extends JLabel {

    private static final long serialVersionUID = 8273875024682878518L;

    private volatile String text;
    private volatile URI uri;
    private volatile LinkStyle inactive;

    /**
     * Constructs a SwingLink with the given text that will launch the given URI
     * when clicked.
     *
     * @throws IllegalArgumentException if uri is not a valid URI
     */
    public SwingLink(String text, String uri) {
        this(text, URI.create(uri));
    }

    /**
     * Constructs a SwingLink with the given text that will launch the given URI
     * when clicked.
     */
    public SwingLink(String text, URI uri) {
        super(text);
        setLink(uri);

        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                open(SwingLink.this.uri);
                inactive = LinkStyle.VISITED;
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                setCursor(new Cursor(Cursor.HAND_CURSOR));
                updateText(LinkStyle.ACTIVE);
            }

            @Override
            public void mouseExited(MouseEvent e) {
                updateText(inactive);
                setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            }
        });
    }

    /**
     * Updates the linked URI, and resets the link style to unvisited.
     */
    public void setLink(URI uri) {
        if (uri == null) {
            throw new NullPointerException();
        }
        this.uri = uri;
        setToolTipText(uri.toString());
        inactive = LinkStyle.UNVISITED;
        updateText(inactive);
    }

    /**
     * Updates the linked URI, and resets the link style to unvisited.
     *
     * @throws IllegalArgumentException if uri is not a valid URI
     */
    public void setLink(String uri) {
        setLink(URI.create(uri));
    }

    /**
     * Styles the text like a link, in addition to the default behavior.
     *
     * {@inheritDoc}
     */
    @Override
    public void setText(String text) {
        if (text == null) {
            throw new NullPointerException();
        }
        this.text = text;
        // inactive is still null when called from JLabel's constructor
        updateText(inactive == null ? LinkStyle.UNVISITED : inactive);
    }

    private void updateText(LinkStyle style) {
        super.setText(style.format(text));
    }

    public URI getLink() {
        return uri;
    }

    public String getLinkText() {
        return text;
    }

    /**
     * Attempts to open a URI in the user's default browser, displaying a
     * graphical warning message if it fails.
     */
    public static void open(URI uri) {
        if (Desktop.isDesktopSupported()) {
            Desktop desktop = Desktop.getDesktop();
            try {
                desktop.browse(uri);
            } catch (IOException e) {
                JOptionPane.showMessageDialog(null,
                        "Failed to open " + uri + " - your computer is likely misconfigured.\n"
                        + "Error Message: " + e.getMessage(),
                        "Cannot Open Link", JOptionPane.WARNING_MESSAGE);
            }
        } else {
            JOptionPane.showMessageDialog(null, "Java is not able to open a browser on your computer.",
                    "Cannot Open Link", JOptionPane.WARNING_MESSAGE);
        }
    }

    private enum LinkStyle {
        UNVISITED(new Color(0x00, 0x00, 0x99), true),
        ACTIVE(new Color(0x99, 0x00, 0x00), false),
        VISITED(new Color(0x80, 0x00, 0x80), true);

        private static final String FORMAT_STRING
                = "<html><span style=\"color: #%02X%02X%02X;\">%s</span></html>";

        private final Color color;
        private final boolean underline;

        LinkStyle(Color c, boolean u) {
            color = c;
            underline = u;
        }

        public String format(String text) {
            String underlinedText = underline ? "<u>" + text + "</u>" : text;
            return String.format(Locale.ROOT, 
                    FORMAT_STRING, color.getRed(), color.getGreen(), color.getBlue(), underlinedText);
        }
    }
}
