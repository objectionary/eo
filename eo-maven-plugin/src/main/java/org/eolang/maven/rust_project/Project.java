/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.maven.rust_project;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.eolang.maven.footprint.Footprint;
import org.eolang.maven.footprint.FtDefault;

/**
 * To create the cargo project.
 * @since 1.0
 */
public final class Project {
    /**
     * Path to cargo project.
     */
    private final Path dest;

    /**
     * Footprint.
     */
    private final Footprint footprint;

    /**
     * Content for Cargo.toml file.
     */
    private final Cargo cargo;

    /**
     * Collection of dependencies of the project.
     */
    private final Set<String> modules;

    /**
     * Ctor.
     * Creates a raw cargo project.
     * @param target Destination path.
     */
    public Project(final Path target) {
        this.footprint = new FtDefault(target);
        this.dest = target;
        this.cargo = new Cargo("common");
        this.modules = new HashSet<>();
    }

    /**
     * Adds the module to the project.
     * @param name Name of function in project.
     * @param raw Content of rust insert.
     * @param crates Dependencies of the module.
     * @throws IOException If any issues with I/O
     */
    public void add(final String name, final String raw, final List<String> crates)
        throws IOException {
        this.modules.add(
            String.format("pub mod %s;", name)
        );
        new Module(raw, name).save(this.footprint);
        for (final String crate: crates) {
            final String[] split = crate.split("[=:]");
            this.cargo.add(split[0], split[1].replace("\"", "").trim());
        }
    }

    /**
     * Saves the project to file system.
     * @return Path to project.
     * @throws IOException If any issues with I/O.
     */
    public Path save() throws IOException {
        this.footprint.save(
            String.format(
                "src%clib",
                File.separatorChar
            ),
            "rs",
            () -> String.join(System.lineSeparator(), this.modules)
        );
        this.cargo.save(this.dest.resolve("Cargo.toml").toFile());
        return this.dest;
    }
}
