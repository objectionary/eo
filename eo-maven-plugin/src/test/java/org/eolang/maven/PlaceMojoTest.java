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

import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang3.ArrayUtils;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link PlaceMojo}.
 *
 * @since 0.11
 */
@SuppressWarnings("PMD.TooManyMethods")
final class PlaceMojoTest {

    /**
     * The default folder for placed binaries.
     */
    private static final String TARGET_CLASSES = "target/classes";

    /**
     * Test library for all binaries.
     */
    private static final String LIBRARY = "foo/hello/-/0.1";

    @Test
    void placesBinaries(@TempDir final Path temp) throws Exception {
        PlaceMojoTest.saveBinary(temp, "EObar/x.bin");
        PlaceMojoTest.saveBinary(temp, "org/eolang/f/x.a.class");
        PlaceMojoTest.saveBinary(temp, "org/eolang/t.txt");
        final Map<String, Path> res = new FakeMaven(temp)
            .execute(PlaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res,
            Matchers.hasKey("target/classes/EObar/x.bin")
        );
        MatcherAssert.assertThat(
            res,
            Matchers.hasKey("target/classes/org/eolang/f/x.a.class")
        );
        MatcherAssert.assertThat(
            res,
            Matchers.hasKey("target/classes/org/eolang/t.txt")
        );
    }

    @Test
    void skipsEoSources(@TempDir final Path temp) throws IOException {
        final String expected = String.format("%s/EObar/x.bin", CopyMojo.DIR);
        PlaceMojoTest.saveBinary(temp, expected);
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .execute(PlaceMojo.class)
                .result(),
            Matchers.not(
                Matchers.hasKey(PlaceMojoTest.expectedTargetClass(expected))
            )
        );
    }

    @Test
    void placesOnlyClassesFromPackageThatHaveSources(@TempDir final Path temp) throws IOException {
        final String sources = String.format("%s/org/eolang/txt/x.eo", CopyMojo.DIR);
        final String[] expected = {
            "EOorg/EOeolang/EOtxt/x.class",
            "EOorg/EOeolang/EOtxt/y&z.class",
            "com/sun/jna/Callback.class",
        };
        final String[] unexpected = {
            "EOorg/EOeolang/EObool.class",
            "EOorg/x.class",
        };
        PlaceMojoTest.saveBinary(temp, sources);
        for (final String binary : ArrayUtils.addAll(expected, unexpected)) {
            PlaceMojoTest.saveBinary(temp, binary);
        }
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .with("placeBinariesThatHaveSources", true)
                .execute(PlaceMojo.class)
                .result(),
            Matchers.allOf(
                Matchers.not(
                    Matchers.hasKey(PlaceMojoTest.expectedTargetClass(sources))
                ),
                Matchers.allOf(
                    Arrays.stream(unexpected)
                        .map(PlaceMojoTest::expectedTargetClass)
                        .map(Matchers::hasKey)
                        .map(Matchers::not)
                        .collect(Collectors.toList())
                ),
                Matchers.allOf(
                    Arrays.stream(expected)
                        .map(PlaceMojoTest::expectedTargetClass)
                        .map(Matchers::hasKey)
                        .collect(Collectors.toList())
                )
            )
        );
    }

    @Test
    void skipsAlreadyPlacedBinaries(@TempDir final Path temp) throws IOException {
        final String binary = "org/eolang/f/x.a.class";
        PlaceMojoTest.saveBinary(temp, binary);
        PlaceMojoTest.saveAlreadyPlacedBinary(temp, binary);
        final long before = PlaceMojoTest.pathToPlacedBinary(
            temp,
            binary
        ).toFile().lastModified();
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withPlacedBinary(
                    temp.resolve(PlaceMojoTest.TARGET_CLASSES).resolve(binary)
                )
                .execute(PlaceMojo.class)
                .result()
                .get("target/classes/org/eolang/f/x.a.class")
                .toFile()
                .lastModified(),
            Matchers.equalTo(before)
        );
    }

    @Test
    void rewritesAlreadyPlacedBinaries(@TempDir final Path temp) throws Exception {
        final String binary = "org/eolang/f/y.a.class";
        final String content = "some new content";
        PlaceMojoTest.saveBinary(temp, content, binary);
        PlaceMojoTest.saveAlreadyPlacedBinary(temp, "old content", binary);
        final Path path = PlaceMojoTest.pathToPlacedBinary(temp, binary);
        final FakeMaven maven = new FakeMaven(temp).withPlacedBinary(path);
        maven.placed().unplaceAll();
        MatcherAssert.assertThat(
            maven.execute(PlaceMojo.class).result(),
            Matchers.hasValue(path)
        );
        MatcherAssert.assertThat(
            content,
            Matchers.is(new TextOf(path).asString())
        );
    }

    @Test
    void placesWithoutBinaries(@TempDir final Path temp) throws IOException {
        Files.createDirectories(temp.resolve("target").resolve(ResolveMojo.DIR));
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .execute(PlaceMojo.class)
                .result(),
            Matchers.not(Matchers.hasKey(PlaceMojoTest.TARGET_CLASSES))
        );
    }

    @Test
    void placesWithoutResolveDirectory(@TempDir final Path temp) throws IOException {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .execute(PlaceMojo.class)
                .result(),
            Matchers.not(Matchers.hasKey(PlaceMojoTest.TARGET_CLASSES))
        );
    }

    @Test
    void placesMissing(@TempDir final Path temp) throws IOException {
        final String first = "EObar/x.bin";
        final String second = "org/eolang/f/x.a.class";
        PlaceMojoTest.saveBinary(temp, first);
        PlaceMojoTest.saveBinary(temp, second);
        PlaceMojoTest.saveAlreadyPlacedBinary(temp, first);
        final Map<String, Path> res = new FakeMaven(temp)
            .execute(PlaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res,
            Matchers.hasValue(PlaceMojoTest.pathToPlacedBinary(temp, first))
        );
        MatcherAssert.assertThat(
            res,
            Matchers.hasValue(PlaceMojoTest.pathToPlacedBinary(temp, second))
        );
    }

    /**
     * Test case for {@link PlaceMojo#execute()}.
     * Since for tests we are using dummy maven central, then instead of unpacking
     * of classes from jar it just copies the jar itself to target/classes folder.
     *
     * @param temp Temporary directory
     * @throws IOException If fails
     */
    @Test
    @ExtendWith(WeAreOnline.class)
    void placesAllEoRuntimeClasses(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            maven.withHelloWorld()
                .execute(new FakeMaven.Place())
                .result()
                .get(PlaceMojoTest.TARGET_CLASSES),
            new ContainsFiles("**/eo-runtime-*.jar")
        );
        MatcherAssert.assertThat(
            maven.placed().jars().size(),
            Matchers.is(1)
        );
    }

    @Test
    void placesWithoutEoRuntimeClasses(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            maven.withHelloWorld()
                .with("withRuntimeDependency", false)
                .execute(new FakeMaven.Place())
                .result()
                .get(PlaceMojoTest.TARGET_CLASSES),
            Matchers.not(new ContainsFiles("**/eo-runtime-*.jar"))
        );
        MatcherAssert.assertThat(
            maven.placed().jars().isEmpty(),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotPlacesAgainIfWasNotUnplaced(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final String binary = "some.class";
        final String old = "some old content";
        PlaceMojoTest.saveBinary(temp, old, binary);
        maven.execute(PlaceMojo.class).result();
        PlaceMojoTest.saveBinary(temp, "new content", binary);
        maven.execute(PlaceMojo.class).result();
        MatcherAssert.assertThat(
            new TextOf(PlaceMojoTest.pathToPlacedBinary(temp, binary)).asString(),
            Matchers.equalTo(old)
        );
    }

    @Test
    void placesAgainIfWasUnplaced(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final String binary = "some.class";
        PlaceMojoTest.saveBinary(temp, "with old content", binary);
        maven.execute(PlaceMojo.class).result();
        final String updated = "with some new content";
        PlaceMojoTest.saveBinary(temp, updated, binary);
        maven.placed().unplaceAll();
        maven.execute(PlaceMojo.class).result();
        MatcherAssert.assertThat(
            new TextOf(PlaceMojoTest.pathToPlacedBinary(temp, binary)).asString(),
            Matchers.equalTo(updated)
        );
    }

    /**
     * Save binary to {@link ResolveMojo#DIR} folder.
     * The method emulates the situation when we have some resolved binaries.
     *
     * @param temp Temp test directory.
     * @param binary Binary name.
     * @throws IOException In case of error.
     */
    private static void saveBinary(final Path temp, final String binary) throws IOException {
        PlaceMojoTest.saveBinary(temp, UUID.randomUUID().toString(), binary);
    }

    /**
     * Save binary to {@link ResolveMojo#DIR} folder.
     * The method emulates the situation when we have some resolved binaries.
     *
     * @param temp Temp test directory.
     * @param content Content of the binary.
     * @param binary Binary name.
     * @throws IOException In case of error.
     */
    private static void saveBinary(
        final Path temp,
        final String content,
        final String binary
    ) throws IOException {
        new HmBase(temp.resolve("target").resolve(ResolveMojo.DIR)).save(
            content,
            Paths.get(String.format("%s/%s", PlaceMojoTest.LIBRARY, binary))
        );
    }

    /**
     * Save binary to classes folder.
     * The method emulates the situation when we already have some placed binaries.
     *
     * @param temp Temp test directory.
     * @param binary Binary name.
     * @throws IOException In case of error.
     */
    private static void saveAlreadyPlacedBinary(
        final Path temp,
        final String binary
    ) throws IOException {
        PlaceMojoTest.saveAlreadyPlacedBinary(temp, UUID.randomUUID().toString(), binary);
    }

    /**
     * Save binary to classes folder.
     * The method emulates the situation when we already have some placed binaries.
     *
     * @param temp Temp test directory.
     * @param content Content of the binary.
     * @param binary Binary name.
     * @throws IOException In case of error.
     */
    private static void saveAlreadyPlacedBinary(
        final Path temp,
        final String content,
        final String binary
    ) throws IOException {
        new HmBase(temp.resolve(PlaceMojoTest.TARGET_CLASSES)).save(content, Paths.get(binary));
    }

    /**
     * Path to the placed binary.
     * @param temp Temp test directory
     * @param binary Binary name.
     * @return Path to the placed binary.
     */
    private static Path pathToPlacedBinary(final Path temp, final String binary) {
        final HmBase home = new HmBase(temp.resolve(PlaceMojoTest.TARGET_CLASSES));
        return home.absolute(Paths.get(binary));
    }

    /**
     * Expected target class.
     * @param expected Expected class.
     * @return Expected target class.
     */
    private static String expectedTargetClass(final String expected) {
        return String.format("%s/%s", PlaceMojoTest.TARGET_CLASSES, expected);
    }
}
