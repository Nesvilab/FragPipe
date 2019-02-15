package umich.msfragger.messages;

import java.util.Properties;

/**
 * This is published when msfragger.properites is obtained from a remote or local location.
 */
public class MessageMsfraggerProps {
  public final Properties props;

  public MessageMsfraggerProps(Properties props) {
    this.props = props;
  }
}
