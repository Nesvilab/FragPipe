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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tools.tmtintegrator;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiConfig.Props;

public class TmtIntegratorTest {
  private static final Logger log = LoggerFactory.getLogger(TmtIntegratorTest.class);

  @Test
  public void configParse() throws IOException {
    final String fn = "tmt-i_param-3.3.1.yml";
    final String resoucePath = "tmtintegrator/" + fn;
    try (InputStream is = this.getClass().getClassLoader().getResourceAsStream(resoucePath)) {
      Yaml yaml = new Yaml(new Constructor(TmtiConfig.class));
      log.debug("Resource stream is: {}", is);
      TmtiConfig loaded = yaml.load(is);
      log.debug("Loaded {}\n:{}", resoucePath, yaml.dump(loaded));
      Assert.assertEquals("D:\\test\\reports", loaded.getTmtintegrator().getOutput());
    }
  }

  @Test
  public void configWrite() throws IOException {
    TmtiConfig config = new TmtiConfig();
    Props props = new Props();
    props.setAdd_Ref(-1);
    props.setAllow_unlabeled(false);
    props.setAllow_overlabel(true);
    props.setBest_psm(true);
    props.setChannel_num(16);
    props.setGroupby(-1);
    props.setMemory(8);
    props.setMin_percent(0.05);
    props.setMin_purity(1);
    props.setMin_site_prob(0);
    config.setTmtintegrator(props);

    Yaml yaml = new Yaml();
    StringWriter writer = new StringWriter();

//    yaml.dump(config, writer);
//    String written = writer.toString();

    String written = yaml.dumpAsMap(config);

    log.debug("Wrote the following yaml contents:\n{}", written);
  }
}
