package org.eolang.maven;

/**
 * Short version of hash.
 *
 * @since 0.28.11
 */
public class HtNarrow implements HashOfTag {

    private final HashOfTag full;

    public HtNarrow(final HtRemote full) {
        this.full = full;
    }

    @Override
    public String hash() {
        return full.hash().substring(0, 7);
    }
}
