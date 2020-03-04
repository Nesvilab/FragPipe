package umich.msfragger.params.tmtintegrator;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

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
    }
  }
}
