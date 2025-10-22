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

public class PythonModule {

  public final String installName;
  public final String someImportName;

  // for split database
  public static final PythonModule NUMPY = new PythonModule("numpy", "numpy");
  public static final PythonModule PANDAS = new PythonModule("pandas", "pandas");

  // for speclib generation
  public static final PythonModule EASYPQP = new PythonModule("fragpipe-speclib", "easypqp");
  public static final PythonModule LXML = new PythonModule("lxml", "lxml");

  public PythonModule(String installName, String someImportName) {
    if (installName == null || someImportName == null) {
      throw new IllegalArgumentException("No nulls");
    }
    this.installName = installName;
    this.someImportName = someImportName;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    PythonModule that = (PythonModule) o;

    if (!installName.equals(that.installName)) {
      return false;
    }
    return someImportName.equals(that.someImportName);
  }

  @Override
  public int hashCode() {
    int result = installName.hashCode();
    result = 31 * result + someImportName.hashCode();
    return result;
  }

  @Override
  public String toString() {
    return "PythonModule{" +
        "installName='" + installName + '\'' +
        ", someImportName='" + someImportName + '\'' +
        '}';
  }
}
