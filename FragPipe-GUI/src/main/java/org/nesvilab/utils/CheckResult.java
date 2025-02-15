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

public class CheckResult {
  public final boolean isSuccess;
  public final String category;
  public final String message;
  public static final String INIT_MSG = "init default";

  public CheckResult(boolean isSuccess, String category, String message) {
    this.isSuccess = isSuccess;
    this.category = category;
    this.message = message;
  }

  public CheckResult(boolean isSuccess, String message) {
    this(isSuccess, null, message);
  }

  public static CheckResult newFail(String category, String message) {
    return new CheckResult(false, category, message);
  }

  public static CheckResult newFail(String category) {
    return new CheckResult(false, category, INIT_MSG);
  }

  public static CheckResult newSuccess(String category, String message) {
    return new CheckResult(true, category, message);
  }

  public final boolean ok() {
    return isSuccess;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    CheckResult that = (CheckResult) o;

    if (isSuccess != that.isSuccess) {
      return false;
    }
    if (category != null ? !category.equals(that.category) : that.category != null) {
      return false;
    }
    return message != null ? message.equals(that.message) : that.message == null;
  }

  @Override
  public int hashCode() {
    int result = (isSuccess ? 1 : 0);
    result = 31 * result + (category != null ? category.hashCode() : 0);
    result = 31 * result + (message != null ? message.hashCode() : 0);
    return result;
  }
}
