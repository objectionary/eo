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

package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.commons.io.FilenameUtils;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Mojo that compiles C native libraries.
 *
 * @since 0.38
 */
@Mojo(
    name = "clib",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class CLibMojo extends SafeMojo {

    /**
     * The directory where the target libraries will be placed.
     */
    private static final Path LIB_TARGET_DIR = Paths.get("Lib/native_clib");

    /**
     * The directory containing sources for using C from EO (e.g. system calls).
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(
        property = "eo.clib",
        required = true,
        defaultValue = "${project.basedir}/src/main/c/eo/lib"
    )
    @SuppressWarnings("PMD.UnusedPrivateField")
    private File cLibDir;

    @Override
    void exec() throws IOException {
        final Path target = this.targetDir.toPath().resolve(CLibMojo.LIB_TARGET_DIR);
        Files.createDirectories(target);
        try (DirectoryStream<Path> files =
            Files.newDirectoryStream(this.cLibDir.toPath(), "*.{c,cpp}")) {
            boolean contains = false;
            for (final Path source : files) {
                contains = true;
                new NativeCLib(
                    source,
                    target.resolve(FilenameUtils.removeExtension(source.getFileName().toString()))
                ).compile();
            }
            if (!contains) {
                throw new IllegalStateException("There are no C sources in the directory");
            }
        }
    }
}
