/*
 * Copyright 2017 dmitriya.
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
package umich.swing.buttons;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;

/**
 *
 * @author dmitriya
 */
public class JGradientButton extends JButton {
    
    private String prop = "prop";

    public String getProp() {
        return prop;
    }

    public void setProp(String prop) {
        this.prop = prop;
    }

    public JGradientButton() {
        setContentAreaFilled(false);
    }

    public JGradientButton(Icon icon) {
        super(icon);
        setContentAreaFilled(false);
    }

    public JGradientButton(String text) {
        super(text);
        setContentAreaFilled(false);
    }

    public JGradientButton(Action a) {
        super(a);
        setContentAreaFilled(false);
    }

    public JGradientButton(String text, Icon icon) {
        super(text, icon);
        setContentAreaFilled(false);
    }
    

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setPaint(new GradientPaint(
                new Point(0, 0),
                getBackground(),
                new Point(0, getHeight() / 3),
                Color.WHITE));
        g2.fillRect(0, 0, getWidth(), getHeight() / 3);
        g2.setPaint(new GradientPaint(
                new Point(0, getHeight() / 3),
                Color.WHITE,
                new Point(0, getHeight()),
                getBackground()));
        g2.fillRect(0, getHeight() / 3, getWidth(), getHeight());
        g2.dispose();

        super.paintComponent(g);
    }
}
