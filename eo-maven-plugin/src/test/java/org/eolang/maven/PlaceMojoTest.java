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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.UUID;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.Home;
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
                Matchers.hasKey(String.format("%s/%s", PlaceMojoTest.TARGET_CLASSES, expected))
            )
        );
    }

    @Test
    void skipsAlreadyPlacedBinaries(@TempDir final Path temp) throws IOException {
        final String binary = "org/eolang/f/x.a.class";
        PlaceMojoTest.saveBinary(temp, binary);
        PlaceMojoTest.saveAlreadyPlacedBinary(temp, binary);
        final long before = PlaceMojoTest.pathToAlreadyPlacedBinary(
            temp,
            binary
        ).toFile().lastModified();
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withPlacedBinary(
                    temp.resolve(PlaceMojoTest.TARGET_CLASSES).resolve(binary).toString()
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
        final Path path = PlaceMojoTest.pathToAlreadyPlacedBinary(temp, binary);
        final Map<String, Path> res = new FakeMaven(temp)
            .withPlacedBinary(path.toString())
            .execute(PlaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            res,
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
            Matchers.hasValue(PlaceMojoTest.pathToAlreadyPlacedBinary(temp, first))
        );
        MatcherAssert.assertThat(
            res,
            Matchers.hasValue(PlaceMojoTest.pathToAlreadyPlacedBinary(temp, second))
        );
    }

    /**
     * Test case for {@link PlaceMojo#execute()}.
     * Since for tests we are using {@link org.eolang.maven.DummyCentral}, then instead of unpacking
     * of classes from jar it just copies the jar itself to target/classes folder.
     *
     * @param temp Temporary directory
     * @throws IOException If fails
     */
    @Test
    @ExtendWith(OnlineCondition.class)
    void placesAllEoRuntimeClasses(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            maven.withHelloWorld()
                .execute(new FakeMaven.Place())
                .result()
                .get(PlaceMojoTest.TARGET_CLASSES),
            new ContainsFile("**/eo-runtime-*.jar")
        );
        MatcherAssert.assertThat(
            maven.placed().select(tojo -> "jar".equals(tojo.get(PlaceMojo.ATTR_PLD_KIND))).size(),
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
            Matchers.not(new ContainsFile("**/eo-runtime-*.jar"))
        );
        MatcherAssert.assertThat(
            maven.placed()
                .select(tojo -> "jar".equals(tojo.get(PlaceMojo.ATTR_PLD_KIND))).isEmpty(),
            Matchers.is(true)
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
        new Home(temp.resolve("target").resolve(ResolveMojo.DIR)).save(
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
        new Home(temp.resolve(PlaceMojoTest.TARGET_CLASSES)).save(content, Paths.get(binary));
    }

    /**
     * Path to the placed binary.
     * @param temp Temp test directory
     * @param binary Binary name.
     * @return Path to the placed binary.
     */
    private static Path pathToAlreadyPlacedBinary(final Path temp, final String binary) {
        final Home home = new Home(temp.resolve(PlaceMojoTest.TARGET_CLASSES));
        return home.absolute(Paths.get(binary));
    }
}
