package umich.msfragger.params.crystalc;

import static org.junit.Assert.*;

import org.junit.Test;

public class CrystalcParamsTest {

  @Test
  public void loadDefault() {
    CrystalcParams p = new CrystalcParams();
    p.loadDefault();
    int a = 1;
  }
}