package umich.msfragger.params.tmtintegrator;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import umich.msfragger.params.tmtintegrator.TmtIntegratorConfig.Props;

public class TmtIntegratorTest {
  private static final Logger log = LoggerFactory.getLogger(TmtIntegratorTest.class);

  @Test
  public void configParse() throws IOException {
    final String fn = "tmt-i_param_v1.1.2.yml";
    final String resoucePath = "tmtintegrator/" + fn;
    try (InputStream is = this.getClass().getClassLoader().getResourceAsStream(resoucePath)) {
      Yaml yaml = new Yaml(new Constructor(TmtIntegratorConfig.class));
      log.debug("Resource stream is: {}", is);
      TmtIntegratorConfig loaded = yaml.load(is);
      log.debug("Loaded {}\n:{}", resoucePath, yaml.dump(loaded));
      Assert.assertEquals("D:\\test\\reports", loaded.getTmtintegrator().getOutput());
    }
  }

  @Test
  public void configWrite() throws IOException {
    TmtIntegratorConfig config = new TmtIntegratorConfig();
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
