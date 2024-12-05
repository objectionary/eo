/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.rust;

import com.moandjiezana.toml.TomlWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;

/**
 * Class to manipulate Cargo.toml file.
 * @since 0.1.0
 */
public final class Cargo extends Savable {
    /**
     * Package attributes.
     */
    private final Map<String, String> pack;

    /**
     * Attributes for [lib].
     */
    private final Map<String, Set<String>> lib;

    /**
     * Dependencies.
     */
    private final Map<String, Object> dependencies;

    /**
     * Ctor.
     * @param name Name of lib.
     */
    public Cargo(final String name) {
        super(
            "Cargo",
            "toml"
        );
        this.pack = new MapOf<>(
            new MapEntry<>("name", name),
            new MapEntry<>("version", "0.1.0"),
            new MapEntry<>("edition", "2021")
        );
        this.lib = new MapOf<>(
            "crate-type", Collections.singleton("cdylib")
        );
        this.dependencies = new MapOf<>(
            "jni", "0.21.1"
        );
    }

    /**
     * Adds dependency.
     * @param crate Dependency name.
     * @param content Dependency description.
     */
    public void add(final String crate, final Object content) {
        this.dependencies.putIfAbsent(crate, content);
    }

    /**
     * Save it to specified folder.
     * @return Content of built cargo.
     * @throws IOException If any issues with I/O
     */
    public String content() {
        final Map<String, Object> raw = new HashMap<>();
        raw.put("package", this.pack);
        raw.put("dependencies", this.dependencies);
        raw.put("lib", this.lib);
        return new TomlWriter().write(
            raw
        );
    }
}
