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

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * To create the cargo project.
 * @since 0.1.0
 */
public final class Project {
    /**
     * Path to cargo project.
     */
    private final Path dest;

    /**
     * Content for Cargo.toml file.
     */
    private final Cargo cargo;

    /**
     * Collection of dependencies of the project.
     */
    private final Set<Module> modules;

    /**
     * Ctor.
     * Creates a raw cargo project.
     * @param target Destination path.
     */
    public Project(final Path target) {
        this.dest = target;
        this.cargo = new Cargo("common");
        this.modules = new HashSet<>();
    }

    /**
     * Adds the module to the project.
     * @param module New module to be added to the project.
     * @param dependencies Dependencies of the module.
     * @return The project.
     */
    public Project with(final Module module, final List<String> dependencies) {
        this.modules.add(
            module
        );
        for (final String crate: dependencies) {
            final String[] split = crate.split("[=:]");
            this.cargo.add(split[0], split[1].replace("\"", "").trim());
        }
        return this;
    }

    /**
     * Add dependency to project. Dependency can be `like jni = "0.21.1"`
     *  or `eo = { path = "/rust/eo" }`.
     * @param name Name of dependency.
     * @param content Content of dependency.
     * @return The project.
     */
    public Project dependency(final String name, final Object content) {
        this.cargo.add(name, content);
        return this;
    }

    /**
     * Saves the project to file system.
     * @return Path to project.
     * @throws IOException If any issues with I/O.
     */
    public Path save() throws IOException {
        for (final Module module: this.modules) {
            new Commented(module, "//").save(this.dest);
        }
        new Commented(this.cargo, "#").save(this.dest);
        return this.dest;
    }
}
