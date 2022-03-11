package com.dmtavt.fragpipe.messages;

public class NoteConfigComet implements INoteConfig {
    public final String bin;
    public final String ver;
    public final Throwable exception;

    public NoteConfigComet(String bin, String ver, Throwable exception) {
        this.bin = bin;
        this.ver = ver;
        this.exception = exception;
    }

    @Override
    public boolean isValid() {
        return exception == null;
    }

    @Override
    public String toString() {
        return "NoteConfigComet{" +
                "bin='" + bin + '\'' +
                ", ver='" + ver + '\'' +
                ", exception=" + exception +
                '}';
    }
}
