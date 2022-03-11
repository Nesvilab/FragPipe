package com.dmtavt.fragpipe.messages;

public class NoteConfigSearchEngine implements INoteConfig {
    public NoteConfigSearchEngine(Type type) {
        this.type = type;
    }

    public enum Type {MsFragger, Comet}

    public final Type type;

    public static Type fromString(String s) {
        for (Type t : Type.values()) {
            if (t.name().equalsIgnoreCase(s))
                return t;
        }
        throw new IllegalArgumentException("Unknown enum constant name for saerch engine type: " + s);
    }

    @Override
    public boolean isValid() {
        return true;
    }
}
