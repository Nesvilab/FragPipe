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
package umich.msfragger.params;

/**
 *
 * @author Dmitry Avtonomov
 */
public abstract class TypedGuiParam<G, F> {
    public final Class<G> clazzGui;
    public final Class<F> clazzFile;
    public final String propName;

    public TypedGuiParam(Class<G> clazzGui, Class<F> clazzFile, String propName) {
        this.clazzGui = clazzGui;
        this.clazzFile = clazzFile;
        this.propName = propName;
    }
    
    public abstract G fromFile(F v);
    public abstract F fromGui(G v);
}
