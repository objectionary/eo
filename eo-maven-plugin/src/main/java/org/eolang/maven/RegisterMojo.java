/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;
import org.eolang.tojos.MonoTojos;
import org.eolang.tojos.Tojos;

/**
 * Register all sources.
 *
 * @since 0.12
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "register",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true
)
@SuppressWarnings("PMD.ImmutableField")
public final class RegisterMojo extends SafeMojo {

    /**
     * Directory in which .eo files are located.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.basedir}/src/main/eo"
    )
    private File sourcesDir;

    /**
     * File with foreign "file objects".
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/foreign.csv"
    )
    private File foreign;

    /**
     * List of inclusion GLOB filters for finding EO files.
     */
    @Parameter
    private Set<String> includes = new SetOf<>("**/*.eo");

    /**
     * List of exclusion GLOB filters for finding EO files.
     */
    @Parameter
    private Set<String> excludes = new HashSet<>(0);

    @Override
    public void exec() throws IOException {
        final Collection<Path> sources = new Walk(this.sourcesDir.toPath())
            .stream()
            .filter(
                file -> this.includes.stream().anyMatch(
                    glob -> RegisterMojo.matcher(glob).matches(file)
                )
            )
            .filter(
                file -> this.excludes.stream().noneMatch(
                    glob -> RegisterMojo.matcher(glob).matches(file)
                )
            )
            .collect(Collectors.toList());
        final Tojos tojos = new MonoTojos(this.foreign);
        final Unplace unplace = new Unplace(this.sourcesDir);
        for (final Path file : sources) {
            tojos
                .add(unplace.make(file))
                .set("version", ParseMojo.ZERO)
                .set("eo", file.toAbsolutePath().toString());
        }
    }

    /**
     * Create glob matcher from text.
     * @param text The pattern
     * @return Matcher
     */
    private static PathMatcher matcher(final String text) {
        return FileSystems.getDefault()
            .getPathMatcher(String.format("glob:%s", text));
    }

}
