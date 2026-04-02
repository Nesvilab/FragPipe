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

package org.nesvilab.fragpipe.messages;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class MessageFragNovoJobInfo {
  public enum JobType {
    FINE_TUNING,
    PREDICTION
  }

  public final JobType jobType;
  public final String url;
  public final String apiKey;
  public final List<String> jobIds;
  public final boolean isRunning;

  public MessageFragNovoJobInfo(JobType jobType, String url, String apiKey, List<String> jobIds, boolean isRunning) {
    this.jobType = jobType;
    this.url = url;
    this.apiKey = apiKey;
    this.jobIds = jobIds != null ? Collections.unmodifiableList(new ArrayList<>(jobIds)) : Collections.emptyList();
    this.isRunning = isRunning;
  }
}
