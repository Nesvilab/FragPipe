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
package org.nesvilab.fragpipe.params;

/**
 *
 * @author dmitriya
 */
public class PropLine {
    String justALine;
    String name;
    String value;
    String comment;

    public String getJustALine() {
        return justALine;
    }

    public String getName() {
        return name;
    }

    public String getValue() {
        return value;
    }

    public String getComment() {
        return comment;
    }
    
    

    public PropLine(String justALine, String name, String value, String comment) {
        this.justALine = justALine;
        this.name = name;
        this.value = value;
        this.comment = comment;
    }

    public boolean isSimpleLine() {
        return justALine != null;
    }

    @Override
    public String toString() {
        if (isSimpleLine())
            return "TextLine{" + justALine + "}";

        return "PropLine{" +
                "name='" + name + '\'' +
                ", value='" + value + '\'' +
                ", comment='" + comment + '\'' +
                '}';
    }
}