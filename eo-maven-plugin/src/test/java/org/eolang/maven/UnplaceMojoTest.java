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

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.file.Path;
import java.security.SecureRandom;
import java.util.Collections;
import java.util.List;
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

    /**
     * Default jar name.
     */
    private static final String DEFAULT_JAR = "eo-lib";

    @Test
    void cleansClasses(@TempDir final Path temp) throws IOException {
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .with("placed", placed.toFile())
                .execute(UnplaceMojo.class)
                .result()
                .values()
                .stream()
                .noneMatch(UnplaceMojoTest::isClass),
            Matchers.is(true)
        );
    }

    @Test
    void cleansBinariesWithJar(@TempDir final Path temp) throws IOException {
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeJar(temp, UnplaceMojoTest.DEFAULT_JAR);
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final List<Tojo> tojos = Catalogs.INSTANCE.make(placed).select(all -> true);
        new FakeMaven(temp)
            .with("placed", placed.toFile())
            .execute(UnplaceMojo.class);
        MatcherAssert.assertThat(
            tojos.size(),
            Matchers.equalTo(5)
        );
        MatcherAssert.assertThat(
            tojos.stream().allMatch(tojo -> tojo.get(PlaceMojo.ATTR_PLD_UNPLACED).equals("true")),
            Matchers.is(true)
        );
    }

    @Test
    void keepsJarBecauseItIsStillInUse(@TempDir final Path temp) throws IOException {
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final String other = "other-jar";
        UnplaceMojoTest.placeJar(temp, other);
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final List<Tojo> tojos = Catalogs.INSTANCE.make(placed).select(all -> true);
        new FakeMaven(temp)
            .with("placed", placed.toFile())
            .execute(UnplaceMojo.class);
        MatcherAssert.assertThat(
            tojos.size(),
            Matchers.equalTo(5)
        );
        MatcherAssert.assertThat(
            tojos.stream()
                .filter(tojo -> tojo.get(Tojos.KEY).equals(other))
                .allMatch(tojo -> tojo.get(PlaceMojo.ATTR_PLD_UNPLACED).equals("false")),
            Matchers.is(true)
        );
    }

    @Test
    void unplacesWithKeepAndRemoveBinariesParamTogether(@TempDir final Path temp) throws Exception {
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final Map<String, Path> res = new FakeMaven(temp)
            .with("placed", placed.toFile())
            .with("keepBinaries", UnplaceMojoTest.GLOB_PATTERN)
            .with("removeBinaries", UnplaceMojoTest.GLOB_PATTERN)
            .execute(UnplaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res.values().stream().noneMatch(UnplaceMojoTest::isClass),
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
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final Map<String, Path> res = new FakeMaven(temp)
            .with("placed", placed.toFile())
            .with("removeBinaries", UnplaceMojoTest.GLOB_PATTERN)
            .execute(UnplaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res.values().stream().noneMatch(UnplaceMojoTest::isClass),
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
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final Map<String, Path> res = new FakeMaven(temp)
            .with("placed", placed.toFile())
            .with("keepBinaries", UnplaceMojoTest.GLOB_PATTERN)
            .execute(UnplaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res.values().stream().anyMatch(UnplaceMojoTest::isClass),
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
     * Saves a class file into the placed tojos file.
     * @param temp Temporary directory.
     * @param clazz Class file.
     * @return Path to the placed tojos file.
     */
    private static Path placeClass(final Path temp, final Path clazz) {
        final Path placed = UnplaceMojoTest.placedFile(temp);
        Catalogs.INSTANCE.make(placed)
            .add(clazz.toString())
            .set(PlaceMojo.ATTR_PLD_KIND, "class")
            .set(PlaceMojo.ATTR_PLD_RELATED, temp.relativize(clazz).toString())
            .set(PlaceMojo.ATTR_PLD_DEP, UnplaceMojoTest.DEFAULT_JAR)
            .set(PlaceMojo.ATTR_PLD_HASH, new FileHash(clazz))
            .set(PlaceMojo.ATTR_PLD_UNPLACED, "false");
        return placed;
    }

    /**
     * Saves a jar into the placed tojos file.
     * @param temp Temporary directory.
     * @param name Name of the jar.
     * @return Path to the placed tojos file.
     */
    private static void placeJar(final Path temp, final String name) {
        Catalogs.INSTANCE.make(UnplaceMojoTest.placedFile(temp))
            .add(name)
            .set(PlaceMojo.ATTR_PLD_KIND, "jar")
            .set(PlaceMojo.ATTR_PLD_DEP, String.format("%s.jar", name))
            .set(PlaceMojo.ATTR_PLD_UNPLACED, "false");
    }

    /**
     * Creates a path to the placed tojos file.
     * @param temp Temporary directory.
     * @return Path to the placed tojos file.
     */
    private static Path placedFile(final Path temp) {
        return temp.resolve("placed.csv");
    }

    /**
     * Creates and saves a class file.
     * @param temp Where to save the file.
     * @return The path to the file.
     * @throws IOException If fails.
     */
    private static Path clazz(final Path temp) throws IOException {
        final Path path = temp.resolve(
            String.format("a/b/c/%d_foo.class", new SecureRandom().nextInt())
        );
        new Home().save(() -> UUID.randomUUID().toString(), path);
        return path;
    }

    /**
     * Checks if the path is a class file.
     * @param path The path to check.
     * @return True if it is a class file.
     */
    private static boolean isClass(final Path path) {
        return path.toString().endsWith(".class");
    }
}

