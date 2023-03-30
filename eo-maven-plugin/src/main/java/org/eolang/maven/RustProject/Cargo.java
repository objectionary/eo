package org.eolang.maven.RustProject;

import com.moandjiezana.toml.TomlWriter;
import org.eolang.maven.footprint.Footprint;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class Cargo {
    private Map<String, String> pack;
    private Map<String, Set<String>> lib;
    private Map<String, String> dependencies;

    public Cargo(String name) {
        this.pack = new HashMap<String, String>() {{
            put("name", name);
            put("version", "0.1.0");
            put("edition", "2021");
        }};
        this.lib = new HashMap<String, Set<String>>() {{
            put("crate-type", Collections.singleton("cdylib"));
        }};
        this.dependencies = new HashMap<String, String>() {{
            put("jni", "0.21.1");
        }};
    }

    public void add(final String crate, final String version) {
        this.dependencies.putIfAbsent(crate, version);
    }

    public void save(final File target) throws IOException {
        Map<String, Object> raw = new HashMap<>();
        raw.put("package", this.pack);
        raw.put("dependencies", this.dependencies);
        raw.put("lib", this.lib);
        new TomlWriter().write(
            raw,
            target
        );
    }
}
