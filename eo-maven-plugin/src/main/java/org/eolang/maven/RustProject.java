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
package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.eolang.maven.footprint.Footprint;
import org.eolang.maven.footprint.FtDefault;

/**
 * To create the cargo project.
 * @since 1.0
 * @todo #1877:90min Transform source rust file by changing name of
 *  function in it to jni standard. It also necessary to add required
 *  headers and imports related to jni.
 * @todo #1877:90min Build the rust project via 'cargo build' command.
 *  Thins command must be called from right directory. Think of correct
 *  compilation errors handling.
 */
public class RustProject {
    /**
     * Path to cargo project.
     */
    private final Path dest;

    /**
     * Footprint.
     */
    private final Footprint footprint;

    /**
     * Collection of dependencies of the project.
     */
    private final Set<String> dependencies;

    /**
     * Ctor.
     * Creates a raw cargo project.
     * @param target Destination path.
     * @throws IOException If any issues with I/O
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public RustProject(final Path target) throws IOException {
        this.footprint = new FtDefault(target);
        this.dest = target;
        this.dependencies = new HashSet<>();
        this.footprint.save(
            "Cargo",
            "toml",
            () ->
                String.join(
                    System.lineSeparator(),
                    "[package]",
                        "name = \"common\"",
                        "version = \"0.1.0\"",
                        "edition = \"2021\"",
                        "[lib]",
                        "crate-type = [\"cdylib\"]",
                        "[dependencies]",
                        "jni = \"0.21.1\""
                   )
        );
        this.footprint.save(
            String.format(
                "src%clib",
                File.separatorChar
            ),
            "rs",
            () -> ""
        );
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
        Files.write(
            this.dest.resolve(Paths.get("src")).resolve("lib.rs"),
            String.format(
                "pub mod %s;%s",
                name,
                System.lineSeparator()
            ).getBytes(),
            StandardOpenOption.APPEND
        );
        this.footprint.save(
            String.format(
                "src%c%s",
                File.separatorChar,
                name
            ),
            "rs",
            () ->
                raw.replaceFirst(
                    String.format(
                        "%s[ ]*pub[ ]+fn[ ]+foo\\(\\)[ ]+->[ ]*Result<u32,[ ]*u32>[ ]*\\{",
                        System.lineSeparator()
                        ),
                    String.format(
                        "%spub fn %s() -> Result<u32> {",
                        System.lineSeparator(),
                        name
                    )
                )
        );
        this.dependencies.addAll(crates);
    }

    /**
     * Compile a project.
     * @return Path to project.
     * @throws IOException If any issues with I/O.
     */
    public Path build() throws IOException {
        Files.write(
            this.dest.resolve("Cargo.toml"),
            String.join(System.lineSeparator(), this.dependencies).getBytes(),
            StandardOpenOption.APPEND
        );
        return this.dest;
    }
}
