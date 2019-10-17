package umich.msfragger.gui;

import java.util.regex.Pattern;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import umich.msfragger.messages.MessageToolInit;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.ISimpleTextComponent;

public interface FragpipeUiHelpers {

  static void messageToTextComponent(ISimpleTextComponent comp, MessageToolInit m) {
    final String old = comp.getText().trim();
    Pattern reHtml = Pattern.compile("<\\s*/?\\s*html\\s*>", Pattern.CASE_INSENSITIVE);
    String noHtml = reHtml.matcher(old).replaceAll("");
    Document doc = Jsoup.parse(old);
    doc.body().attr("style", SwingUtils.getHtmlBodyStyle());
    if (!m.append) {
      doc.body().html("");
    }
    if (m.isError) {
      doc.body().appendChild(new Element("i").html(m.text));
    } else {
      doc.body().append(m.text);
    }
    String html = doc.html();
    comp.setText(html);
  }
}
