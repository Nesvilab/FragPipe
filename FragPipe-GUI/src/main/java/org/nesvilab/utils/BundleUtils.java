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
package org.nesvilab.utils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class BundleUtils {
    
    private BundleUtils() {}
    
    
    /**
     * It's here just so not to forget how to get bundles
     * @param path Relative to the root source folder. E.g. 
     * "org/nesvilab/fragpipe/Bundle". As you can see, it has the word "Bundle"
     * but it's not followed by ".properties".
     * @return 
     */
    public static java.util.ResourceBundle getBundle(String path) {
        return java.util.ResourceBundle.getBundle(path); // NOI18N
    }
}
