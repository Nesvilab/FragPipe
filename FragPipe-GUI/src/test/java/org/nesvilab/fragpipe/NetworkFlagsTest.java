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

package org.nesvilab.fragpipe;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.fragpipe.tools.diatracer.DiaTracer;
import org.nesvilab.fragpipe.tools.fragger.Msfragger;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerProps;
import org.nesvilab.fragpipe.tools.ionquant.IonQuant;

/**
 * Verifies that {@code NetworkFlags.ENABLE_NETWORK = false} actually prevents FragPipe from
 * attempting any outbound connection on the gated code paths.
 *
 * <p>Approach: install a {@link ProxySelector} that records every URI the JVM asks about
 * before opening a socket. {@code URLConnection.connect()} invokes {@code ProxySelector.select(URI)}
 * before any DNS lookup or socket attempt, so the recorder observes the intent to connect
 * regardless of whether the network is reachable. All of FragPipe's gated paths go through
 * {@code java.net.URL} / {@code URLConnection}, so every would-be attempt is caught.
 */
public class NetworkFlagsTest {

  private static final class RecordingProxySelector extends ProxySelector {
    private final List<URI> selected = Collections.synchronizedList(new ArrayList<>());

    @Override
    public List<Proxy> select(URI uri) {
      selected.add(uri);
      return Collections.singletonList(Proxy.NO_PROXY);
    }

    @Override
    public void connectFailed(URI uri, SocketAddress sa, IOException ioe) {
      // no-op
    }

    List<URI> snapshot() {
      synchronized (selected) {
        return new ArrayList<>(selected);
      }
    }

    void clear() {
      selected.clear();
    }
  }

  private ProxySelector originalSelector;
  private RecordingProxySelector recorder;
  private boolean originalEnableNetwork;

  @Before
  public void install() {
    originalSelector = ProxySelector.getDefault();
    recorder = new RecordingProxySelector();
    ProxySelector.setDefault(recorder);
    originalEnableNetwork = NetworkFlags.ENABLE_NETWORK;
  }

  @After
  public void restore() {
    NetworkFlags.ENABLE_NETWORK = originalEnableNetwork;
    ProxySelector.setDefault(originalSelector);
  }

  /**
   * Sanity check: confirms the recorder actually observes outbound URL attempts. Uses a
   * TEST-NET-1 address (RFC 5737) so no real host is contacted. {@code ProxySelector.select}
   * fires before any socket attempt, so the URI is captured even though the connect fails.
   */
  @Test
  public void recordingProxySelectorObservesOutboundAttempts() throws IOException {
    java.net.URLConnection conn = new URL("http://192.0.2.1/probe").openConnection();
    conn.setConnectTimeout(250); // keep the fail-fast path short
    conn.setReadTimeout(250);
    try {
      conn.getInputStream().close();
    } catch (IOException expected) {
      // connection will fail; we only care that ProxySelector was consulted beforehand
    }
    List<URI> attempts = recorder.snapshot();
    assertFalse("positive control: recorder should have seen the outbound URI",
        attempts.isEmpty());
    assertEquals("192.0.2.1", attempts.get(0).getHost());
  }

  @Test
  public void flagDefaultsToTrue() {
    assertTrue("ENABLE_NETWORK should default to true in source", originalEnableNetwork);
  }

  @Test
  public void flagIsRuntimeMutable() {
    NetworkFlags.ENABLE_NETWORK = false;
    assertFalse(NetworkFlags.ENABLE_NETWORK);
    NetworkFlags.ENABLE_NETWORK = true;
    assertTrue(NetworkFlags.ENABLE_NETWORK);
  }

  /**
   * Covers the startup remote-properties fetch (fragpipe.info) and, transitively, the
   * {@code TabConfig.createPanelBottomLink} and {@code Fragpipe.createAboutBody} call sites,
   * since both reach {@link ThisAppProps#getRemoteProperties()}.
   */
  @Test
  public void thisAppPropsRemoteDoesNotAttemptConnection() {
    NetworkFlags.ENABLE_NETWORK = false;
    recorder.clear();

    Properties p = ThisAppProps.getRemoteProperties();

    assertNull("getRemoteProperties() must return null when network is disabled", p);
    assertNoOutbound();
  }

  @Test
  public void thisAppPropsMergedDoesNotAttemptConnection() {
    NetworkFlags.ENABLE_NETWORK = false;
    recorder.clear();

    Properties merged = ThisAppProps.getRemotePropertiesWithLocalDefaults();

    assertNotNull(merged);
    assertNotNull("local Bundle.properties should still be merged in",
        merged.getProperty("msfragger.gui.version"));
    assertNoOutbound();
  }

  @Test
  public void msfraggerPropsRemoteDoesNotAttemptConnection() {
    NetworkFlags.ENABLE_NETWORK = false;
    recorder.clear();

    assertNull(MsfraggerProps.getRemoteProperties());
    assertNoOutbound();
  }

  /**
   * {@link Msfragger#checkUpdates} normally spawns a thread that hits
   * msfragger.nesvilab.org. With the flag off, the gate returns before the argument is
   * dereferenced, so {@code null} is a valid input here.
   */
  @Test
  public void msfraggerCheckUpdatesDoesNotAttemptConnection() throws InterruptedException {
    NetworkFlags.ENABLE_NETWORK = false;
    recorder.clear();

    Msfragger.checkUpdates(null);
    Thread.sleep(150); // belt-and-suspenders: catch any rogue worker thread

    assertNoOutbound();
  }

  @Test
  public void ionquantCheckUpdatesDoesNotAttemptConnection() throws InterruptedException {
    NetworkFlags.ENABLE_NETWORK = false;
    recorder.clear();

    IonQuant.checkUpdates(null);
    Thread.sleep(150);

    assertNoOutbound();
  }

  @Test
  public void diatracerCheckUpdatesDoesNotAttemptConnection() throws InterruptedException {
    NetworkFlags.ENABLE_NETWORK = false;
    recorder.clear();

    DiaTracer.checkUpdates(null);
    Thread.sleep(150);

    assertNoOutbound();
  }

  private void assertNoOutbound() {
    List<URI> attempts = recorder.snapshot();
    assertTrue("Expected zero outbound network attempts, but recorded: " + attempts,
        attempts.isEmpty());
  }
}
