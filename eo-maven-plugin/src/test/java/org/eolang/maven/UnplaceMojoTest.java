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

import java.io.IOException;
import java.nio.file.Path;
import java.security.SecureRandom;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.FileHash;
import org.eolang.maven.util.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link UnplaceMojo}.
 *
 * @since 0.1
 * @checkstyle LocalFinalVariableNameCheck (100 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class UnplaceMojoTest {

    /**
     * Binary glob pattern.
     */
    private static final Set<String> GLOB_PATTERN = Collections.singleton("**.class");

    @Test
    void cleans(@TempDir final Path temp) throws IOException {
        UnplaceMojoTest.placed(temp, UnplaceMojoTest.binary(temp));
        UnplaceMojoTest.placed(temp, UnplaceMojoTest.binary(temp));
        UnplaceMojoTest.placed(temp, UnplaceMojoTest.binary(temp));
        final Path placed = UnplaceMojoTest.placed(temp, UnplaceMojoTest.binary(temp));
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .with("placed", placed.toFile())
                .execute(UnplaceMojo.class)
                .result()
                .values()
                .stream()
                .noneMatch(UnplaceMojoTest::isBinary),
            Matchers.is(true)
        );
    }

    @Test
    void unplacesWithKeepAndRemoveBinariesParamTogether(@TempDir final Path temp) throws Exception {
        final Path placed = UnplaceMojoTest.placed(temp, UnplaceMojoTest.binary(temp));
        final Map<String, Path> res = new FakeMaven(temp)
            .with("placed", placed.toFile())
            .with("keepBinaries", UnplaceMojoTest.GLOB_PATTERN)
            .with("removeBinaries", UnplaceMojoTest.GLOB_PATTERN)
            .execute(UnplaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res.values().stream().noneMatch(UnplaceMojoTest::isBinary),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(placed.getFileName().toString())).asString(),
            Matchers.allOf(
                Matchers.not(Matchers.containsString("false")),
                Matchers.containsString("true")
            )
        );
    }

    @Test
    void unplacesWithRemoveBinariesParam(@TempDir final Path temp) throws Exception {
        final Path placed = UnplaceMojoTest.placed(temp, UnplaceMojoTest.binary(temp));
        final Map<String, Path> res = new FakeMaven(temp)
            .with("placed", placed.toFile())
            .with("removeBinaries", UnplaceMojoTest.GLOB_PATTERN)
            .execute(UnplaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res.values().stream().noneMatch(UnplaceMojoTest::isBinary),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(placed.getFileName().toString())).asString(),
            Matchers.allOf(
                Matchers.not(Matchers.containsString("false")),
                Matchers.containsString("true")
            )
        );
    }

    @Test
    void unplacesWithKeepBinariesParam(@TempDir final Path temp) throws Exception {
        final Path placed = UnplaceMojoTest.placed(temp, UnplaceMojoTest.binary(temp));
        final Map<String, Path> res = new FakeMaven(temp)
            .with("placed", placed.toFile())
            .with("keepBinaries", UnplaceMojoTest.GLOB_PATTERN)
            .execute(UnplaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res.values().stream().anyMatch(UnplaceMojoTest::isBinary),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(placed.getFileName().toString())).asString(),
            Matchers.allOf(
                Matchers.containsString("false"),
                Matchers.not(Matchers.containsString("true"))
            )
        );
    }

    /**
     * Saves a binary file into the placed tojos file.
     * @param temp Temporary directory.
     * @param binary Binary file.
     * @return Path to the placed tojos file.
     */
    private static Path placed(final Path temp, final Path binary) {
        final Path placed = temp.resolve("placed.csv");
        Catalogs.INSTANCE.make(placed)
            .add(binary.toString())
            .set(PlaceMojo.ATTR_PLD_KIND, "class")
            .set(PlaceMojo.ATTR_PLD_RELATED, temp.relativize(binary).toString())
            .set(PlaceMojo.ATTR_PLD_ORIGIN, "some-keep-remove.jar")
            .set(PlaceMojo.ATTR_PLD_HASH, new FileHash(binary))
            .set(PlaceMojo.ATTR_PLD_UNPLACED, "false");
        return placed;
    }

    /**
     * Creates and saves a binary file.
     * @param temp Where to save the file.
     * @return The path to the file.
     * @throws IOException If fails.
     */
    private static Path binary(final Path temp) throws IOException {
        final Path foo = temp.resolve(
            String.format("a/b/c/%d_foo.class", new SecureRandom().nextInt())
        );
        new Home().save(() -> UUID.randomUUID().toString(), foo);
        return foo;
    }

    /**
     * Checks if the path is a binary file.
     * @param path The path to check.
     * @return True if it is a binary file.
     */
    private static boolean isBinary(final Path path) {
        return path.toString().endsWith(".class");
    }
}

