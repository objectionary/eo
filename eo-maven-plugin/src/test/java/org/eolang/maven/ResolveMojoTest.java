/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ResolveMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class ResolveMojoTest {

    @Test
    void testSimpleResolve(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("src");
        new Home(src).save(
            String.format(
                "%s\n%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.7.0",
                "+rt jvm org.eolang:eo-sys:jar-with-dependencies:0.0.5",
                "[] > foo /int"
            ),
            Paths.get("foo.eo")
        );
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        new Moja<>(ParseMojo.class)
            .with("foreign", foreign.toFile())
            .with("targetDir", target.toFile())
            .with("cache", temp.resolve("cache/parsed"))
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .execute();
        new Moja<>(ResolveMojo.class)
            .with("foreign", foreign.toFile())
            .with("targetDir", target.toFile())
            .with("central", Central.EMPTY)
            .execute();
        MatcherAssert.assertThat(
            true,
            Matchers.is(true)
        );
    }

    /**
     * Test conflicts.
     *
     * @param temp Temp folder
     * @throws IOException In case of I/O issues.
     */
    @Test
    void testConflictingDependencies(@TempDir final Path temp) throws IOException {
        final Path first = temp.resolve("src/foo1.src");
        new Home(temp).save(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:0.22.1",
                "[] > foo /int"
            ),
            temp.relativize(first)
        );
        final Path second = temp.resolve("src/foo2.src");
        new Home(temp).save(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:0.22.0",
                "[] > foo /int"
            ),
            temp.relativize(second)
        );
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        Catalogs.INSTANCE.make(foreign, "json")
            .add("foo1.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, first.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.22.1");
        Catalogs.INSTANCE.make(foreign, "json")
            .add("foo2.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, second.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.22.0");
        new Moja<>(ParseMojo.class)
            .with("foreign", foreign.toFile())
            .with("targetDir", target.toFile())
            .with("cache", temp.resolve("cache/parsed"))
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .execute();
        final Exception excpt = Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Moja<>(ResolveMojo.class)
                .with("foreign", foreign.toFile())
                .with("targetDir", target.toFile())
                .with("central", Central.EMPTY)
                .with("skipZeroVersions", true)
                .with("discoverSelf", false)
                .with("ignoreVersionConflicts", false)
                .execute()
        );
        MatcherAssert.assertThat(
            excpt.getMessage(),
            Matchers.equalTo(
                new StringBuilder("1 conflicting dependencies are found: ")
                    .append("{org.eolang:eo-runtime:jar:=[0.22.0, 0.22.1]}")
                    .toString()
            )
        );
    }

    @Test
    void testConflictingDependenciesNoFail(@TempDir final Path temp) throws IOException {
        final Path first = temp.resolve("src/foo1.src");
        new Home(temp).save(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.22.1",
                "[] > foo /int"
            ),
            temp.relativize(first)
        );
        final Path second = temp.resolve("src/foo2.src");
        new Home(temp).save(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.22.0",
                "[] > foo /int"
            ),
            temp.relativize(second)
        );
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign");
        Catalogs.INSTANCE.make(foreign)
            .add("foo1.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, first.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.22.1");
        Catalogs.INSTANCE.make(foreign)
            .add("foo2.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, second.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.22.0");
        new Moja<>(ParseMojo.class)
            .with("foreign", foreign.toFile())
            .with("targetDir", target.toFile())
            .with("cache", temp.resolve("cache/parsed"))
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .execute();
        new Moja<>(ResolveMojo.class)
            .with("foreign", foreign.toFile())
            .with("targetDir", target.toFile())
            .with("central", Central.EMPTY)
            .with("skipZeroVersions", true)
            .with("discoverSelf", false)
            .with("ignoreVersionConflicts", true)
            .with("ignoreTransitive", true)
            .execute();
        MatcherAssert.assertThat(
            true,
            Matchers.equalTo(true)
        );
    }
}
