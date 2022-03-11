package com.dmtavt.fragpipe.messages;

public class NoteConfigCometParams {
    public final int decoy_search;
    public final boolean dbHasDecoys;

    public NoteConfigCometParams(int decoy_search, boolean dbHasDecoys) {
        this.decoy_search = decoy_search;
        this.dbHasDecoys = dbHasDecoys;
    }
}
