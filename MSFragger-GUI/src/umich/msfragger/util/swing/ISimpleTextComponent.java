package umich.msfragger.util.swing;

import javax.swing.JLabel;
import javax.swing.text.JTextComponent;
import umich.msfragger.gui.MsfraggerGuiFrame;

public interface ISimpleTextComponent {
  String getText();
  void setText(String text);

  public static ISimpleTextComponent fromJLabel(final JLabel comp) {
    return new ISimpleTextComponent() {
      @Override
      public String getText() {
        return comp.getText();
      }

      @Override
      public void setText(String text) {
        comp.setText(text);
      }
    };
  }

  public static ISimpleTextComponent fromTextComponent(final JTextComponent comp) {
    return new ISimpleTextComponent() {
      @Override
      public String getText() {
        return comp.getText();
      }

      @Override
      public void setText(String text) {
        comp.setText(text);
      }
    };
  }
}
