/* 
 * Copyright 2016 Dmitry Avtonomov.
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
package umich.msfragger.params;

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