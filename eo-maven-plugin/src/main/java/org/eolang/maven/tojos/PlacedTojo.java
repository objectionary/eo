package org.eolang.maven.tojos;

import com.yegor256.tojos.Tojo;

public class PlacedTojo implements Tojo {

    private final Tojo origin;

    public PlacedTojo(final Tojo tojo) {
        this.origin = tojo;
    }

    public String id() {
        return this.origin.get(PlacedTojos.Attribute.ID.key());
    }

    public String dependency() {
        return this.origin.get(PlacedTojos.Attribute.DEPENDENCY.key());
    }

    public String related() {
        return this.origin.get(PlacedTojos.Attribute.RELATED.key());
    }

    public boolean sameHash(final String hash) {
        return this.origin.get(PlacedTojos.Attribute.HASH.key()).equals(hash);
    }

    public void unplace() {
        this.origin.set(PlacedTojos.Attribute.UNPLACED.key(), "true");
    }

    public String unplaced() {
        return this.origin.get(PlacedTojos.Attribute.UNPLACED.key());
    }

    @Override
    public boolean exists(final String key) {
        return this.origin.exists(key);
    }

    @Override
    public String get(final String key) {
        return this.origin.get(key);
    }

    @Override
    public Tojo set(final String key, final Object value) {
        return this.origin.set(key, value);
    }
}
