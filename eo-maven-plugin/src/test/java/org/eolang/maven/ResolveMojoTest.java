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

import com.yegor256.tojos.Csv;
import com.yegor256.tojos.MonoTojos;
import java.io.IOException;
import java.nio.file.Path;
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
public final class ResolveMojoTest {

    @Test
    public void testSimpleResolve(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("src");
        new Save(
            "+rt jvm org.eolang:eo-runtime:0.7.0\n\n[] > foo /int\n",
            src.resolve("foo.eo")
        ).save();
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        new Moja<>(ParseMojo.class)
            .with("foreign", foreign.toFile())
            .with("targetDir", target.toFile())
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
     * @param temp Temp folder
     * @throws IOException In case of I/O issues.
     */
    @Test
    public void testConflictingDependencies(@TempDir final Path temp) throws IOException {
        final Path first = temp.resolve("src/foo1.src");
        new Save(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:0.22.1",
                "[] > foo /int"
            ),
            first
        ).save();
        final Path second = temp.resolve("src/foo2.src");
        new Save(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:0.22.0",
                "[] > foo /int"
            ),
            second
        ).save();
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        new MonoTojos(new Csv(foreign))
            .add("foo1.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, first.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.22.1");
        new MonoTojos(new Csv(foreign))
            .add("foo2.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, second.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.22.0");
        new Moja<>(ParseMojo.class)
            .with("foreign", foreign.toFile())
            .with("targetDir", target.toFile())
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
                "1 conflicting dependencies are found: {org.eolang:eo-runtime:jar:=[0.22.0, 0.22.1]}"
            )
        );
    }

    @Test
    public void testConflictingDependenciesNoFail(@TempDir final Path temp) throws IOException {
        final Path first = temp.resolve("src/foo1.src");
        new Save(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:0.22.1",
                "[] > foo /int"
            ),
            first
        ).save();
        final Path second = temp.resolve("src/foo2.src");
        new Save(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:0.22.0",
                "[] > foo /int"
            ),
            second
        ).save();
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        new MonoTojos(new Csv(foreign))
            .add("foo1.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, first.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.22.1");
        new MonoTojos(new Csv(foreign))
            .add("foo2.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, second.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.22.0");
        new Moja<>(ParseMojo.class)
            .with("foreign", foreign.toFile())
            .with("targetDir", target.toFile())
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
            .execute();
        MatcherAssert.assertThat(
            true,
            Matchers.equalTo(true)
        );
    }
}
