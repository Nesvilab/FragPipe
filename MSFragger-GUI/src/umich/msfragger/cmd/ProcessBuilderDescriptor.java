package umich.msfragger.cmd;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ProcessBuilderDescriptor {
  public final List<ProcessBuilder> pbs;
  public final String name;
  public final int priority;

  public ProcessBuilderDescriptor(String name) {
    this(name, 100);
  }

  public ProcessBuilderDescriptor(String name, int priority) {
    this.name = name;
    this.priority = priority;
    this.pbs = new ArrayList<>();
  }

  public int size() {
    return pbs.size();
  }

  public boolean isEmpty() {
    return pbs.isEmpty();
  }

  public ProcessBuilderDescriptor add(ProcessBuilder processBuilder) {
    pbs.add(processBuilder);
    return this;
  }

  public ProcessBuilderDescriptor addAll(Collection<? extends ProcessBuilder> c) {
    pbs.addAll(c);
    return this;
  }
}
