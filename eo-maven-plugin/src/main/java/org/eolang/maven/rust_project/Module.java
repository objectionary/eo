package org.eolang.maven.rust_project;

import org.eolang.maven.footprint.Footprint;

/**
 * Special class for converting a rust inserts
 * into a separate module of Cargo Project.
 */
public class Module {
    private final String raw;
    private final String name;
    private String transformed;
    public Module(final String raw, final String name) {
        this.raw = raw;
        this.name = name;
    }

    public void save(final Footprint footprint) {

    }

    private void transform() {
        String function = "Java_EOorg_EOeolang_EOrust_".concat(this.name);
    }
}
