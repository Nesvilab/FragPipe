package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.UpdatePackage;
import com.dmtavt.fragpipe.messages.MessageUpdatePackagesAvailable;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.VersionComparator;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FragpipeUpdater {
  private static final Logger log = LoggerFactory.getLogger(FragpipeUpdater.class);

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageUpdatePackagesAvailable m) {
    log.debug("Got MessageUpdatePackagesAvailable");
  }

  public static List<UpdatePackage> checkNewUpdatePackages(Properties props) {
    Map<String, String> updates = Seq.seq(props.stringPropertyNames())
        .filter(prop -> prop.startsWith("update-package."))
        .filter(prop -> !prop.contains(".desc"))
        .toMap(s -> s, s -> props.getProperty(s));

    List<UpdatePackage> toDownload = new ArrayList<>();
    for (Entry<String, String> kv : updates.entrySet()) {
      String propName = kv.getKey();
      String url = kv.getValue();
      String[] split = url.split("/");
      if (split.length < 2) {
        log.warn("Likely error in remote updates file: " + propName);
        continue;
      }
      String fn = split[split.length - 1];
      Path dest = FragpipeLocations.get().getOrMakeInAppDir("updates");
      Optional<Path> ours = PathUtils
          .findFilesQuietly(dest, path -> path.getFileName().toString().equals(fn)).findAny();
      if (ours.isPresent()) {
        log.debug("We already have file from remote: {}", dest.relativize(ours.get()));
      } else {
        String desc = props.getProperty(propName + ".desc", "");
        Pattern re = Pattern.compile("\\.ver\\[([^],]+?),([^]]*?)]");
        String minVer = "";
        String maxVer = "";
        Matcher m = re.matcher(propName);
        boolean isWithinRange = true;
        if (m.find()) {
          minVer = m.group(1);
          maxVer = m.group(2);
          // check if current version is within range
          VersionComparator vc = new VersionComparator();
          if (isWithinRange && !StringUtils.isBlank(minVer)) {
            isWithinRange = vc.compare(Version.version(), minVer) >= 0;
          }
          if (isWithinRange && !StringUtils.isBlank(maxVer)) {
            isWithinRange = vc.compare(Version.version(), maxVer) <= 0;
          }
        }
        if (isWithinRange) {
          toDownload.add(new UpdatePackage(kv.getValue(), propName, desc, minVer, maxVer));
        } else {
          log.debug("Update package '{}' is out of version range, cur ver '{}'", propName, Version.version());
        }
      }
    }
    if (toDownload.isEmpty()) {
      log.debug("No update packages to download");
    } else {
      log.debug("Need to download update packages:\n\t{}", Seq.seq(toDownload).toString("\n\t"));
    }
    return toDownload;
  }
}
