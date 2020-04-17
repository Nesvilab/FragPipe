package com.dmtavt.fragpipe.params.fragger;

import static org.junit.Assert.assertFalse;

import java.util.List;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EnzymeProviderTest {
  private static final Logger log = LoggerFactory.getLogger(EnzymeProviderTest.class);

  @Test
  public void get() {
    List<MsfraggerEnzyme> enzymes = new EnzymeProvider().get();
    log.info("Parsed enzymes: {}", enzymes);
    assertFalse("No enzymes parsed", enzymes.isEmpty());
  }
}