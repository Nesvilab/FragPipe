package com.dmtavt.fragpipe.api;

import java.nio.file.Path;
import java.util.List;

public interface IPathsProvider {
  List<Path> get();
}
