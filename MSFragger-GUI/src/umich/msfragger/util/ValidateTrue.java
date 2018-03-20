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

/**
 *
 * @author Dmitry Avtonomov
 */
public class ValidateTrue implements IValidateString {
    
    private ValidateTrue() {
    }
    
    public static ValidateTrue getInstance() {
        return ValidateTrueHolder.INSTANCE;
    }

    private static class ValidateTrueHolder {
        private static final ValidateTrue INSTANCE = new ValidateTrue();
    }
    
    @Override
    public boolean test(String s) {
        return true;
    }
}
