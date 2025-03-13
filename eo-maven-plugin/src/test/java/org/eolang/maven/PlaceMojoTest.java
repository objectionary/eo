/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link PlaceMojo}.
 *
 * @since 0.11
 */
@SuppressWarnings("PMD.TooManyMethods")
@ExtendWith(MktmpResolver.class)
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
    void placesBinaries(@Mktmp final Path temp) throws Exception {
        PlaceMojoTest.saveBinary(temp, "EObar/x.bin");
        PlaceMojoTest.saveBinary(temp, "org/eolang/f/x.a.class");
        PlaceMojoTest.saveBinary(temp, "org/eolang/t.txt");
        MatcherAssert.assertThat(
            "PlaceMojo have to place binaries, but it doesn't",
            new FakeMaven(temp)
                .execute(PlaceMojo.class)
                .result(),
            Matchers.allOf(
                Matchers.hasKey("target/classes/EObar/x.bin"),
                Matchers.hasKey("target/classes/org/eolang/f/x.a.class"),
                Matchers.hasKey("target/classes/org/eolang/t.txt")
            )
        );
    }

    @Test
    void skipsAlreadyPlacedBinaries(@Mktmp final Path temp) throws IOException {
        final String binary = "org/eolang/f/x.a.class";
        PlaceMojoTest.saveBinary(temp, binary);
        PlaceMojoTest.saveAlreadyPlacedBinary(temp, binary);
        final long before = PlaceMojoTest.pathToPlacedBinary(
            temp,
            binary
        ).toFile().lastModified();
        MatcherAssert.assertThat(
            "PlaceMojo must skip already placed binaries, but it doesn't",
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
    void rewritesAlreadyPlacedBinaries(@Mktmp final Path temp) throws Exception {
        final String binary = "org/eolang/f/y.a.class";
        final String content = "some new content";
        PlaceMojoTest.saveBinary(temp, content, binary);
        PlaceMojoTest.saveAlreadyPlacedBinary(temp, "old content", binary);
        final Path path = PlaceMojoTest.pathToPlacedBinary(temp, binary);
        final FakeMaven maven = new FakeMaven(temp).withPlacedBinary(path);
        maven.placed().unplaceAll();
        MatcherAssert.assertThat(
            "PlaceMojo have to process the file",
            maven.execute(PlaceMojo.class).result(),
            Matchers.hasValue(path)
        );
        MatcherAssert.assertThat(
            "The file must be updated, but it was not",
            content,
            Matchers.is(new TextOf(path).asString())
        );
    }

    @Test
    void placesWithoutBinaries(@Mktmp final Path temp) throws IOException {
        Files.createDirectories(temp.resolve("target").resolve(ResolveMojo.DIR));
        MatcherAssert.assertThat(
            String.format(
                "PlaceMojo must not place binaries from %s",
                PlaceMojoTest.TARGET_CLASSES
            ),
            new FakeMaven(temp)
                .execute(PlaceMojo.class)
                .result(),
            Matchers.not(Matchers.hasKey(PlaceMojoTest.TARGET_CLASSES))
        );
    }

    @Test
    void placesDefaultJnaBinaries(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "PlaceMojo have to place default Jna binary",
            new FakeMaven(temp)
                .withHelloWorld()
                .with("withRuntimeDependency", false)
                .execute(new FakeMaven.Place())
                .result()
                .get(PlaceMojoTest.TARGET_CLASSES),
            new ContainsFiles("**/jna-*.jar")
        );
    }

    @Test
    void placesWithoutResolveDirectory(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            String.format(
                String.join(
                    " ",
                    "PlaceMojo must not place binaries from %s",
                    "if the resolve directory does not exist"
                ),
                PlaceMojoTest.TARGET_CLASSES
            ),
            new FakeMaven(temp)
                .execute(PlaceMojo.class)
                .result(),
            Matchers.not(Matchers.hasKey(PlaceMojoTest.TARGET_CLASSES))
        );
    }

    @Test
    void placesMissing(@Mktmp final Path temp) throws IOException {
        final String first = "EObar/x.bin";
        final String second = "org/eolang/f/x.a.class";
        PlaceMojoTest.saveBinary(temp, first);
        PlaceMojoTest.saveBinary(temp, second);
        PlaceMojoTest.saveAlreadyPlacedBinary(temp, first);
        MatcherAssert.assertThat(
            "The first binary file must be placed, but it was not",
            new FakeMaven(temp)
                .execute(PlaceMojo.class)
                .result(),
            Matchers.allOf(
                Matchers.hasValue(PlaceMojoTest.pathToPlacedBinary(temp, first)),
                Matchers.hasValue(PlaceMojoTest.pathToPlacedBinary(temp, second))
            )
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
    void placesAllEoRuntimeClasses(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            "PlaceMojo have to place the runtime file, but doesn't",
            maven.withHelloWorld()
                .with("resolveJna", false)
                .execute(new FakeMaven.Place())
                .result()
                .get(PlaceMojoTest.TARGET_CLASSES),
            new ContainsFiles("**/eo-runtime-*.jar")
        );
        MatcherAssert.assertThat(
            "PlaceMojo have to place jar file, but doesn't",
            maven.placed().jars().size(),
            Matchers.is(1)
        );
    }

    @Test
    void placesWithoutEoRuntimeClasses(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            "PlaceMojo have not to place the runtime file, but doesn't",
            maven.withHelloWorld()
                .with("withRuntimeDependency", false)
                .with("resolveJna", false)
                .execute(new FakeMaven.Place())
                .result()
                .get(PlaceMojoTest.TARGET_CLASSES),
            Matchers.not(new ContainsFiles("**/eo-runtime-*.jar"))
        );
        MatcherAssert.assertThat(
            "PlaceMojo have not to place jar file, but doesn't",
            maven.placed().jars().isEmpty(),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotPlacesAgainIfWasNotUnplaced(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final String binary = "some.class";
        final String old = "some old content";
        PlaceMojoTest.saveBinary(temp, old, binary);
        maven.execute(PlaceMojo.class).result();
        PlaceMojoTest.saveBinary(temp, "new content", binary);
        maven.execute(PlaceMojo.class).result();
        MatcherAssert.assertThat(
            "The binary file must not be replaced with new content, but it was not",
            new TextOf(PlaceMojoTest.pathToPlacedBinary(temp, binary)).asString(),
            Matchers.equalTo(old)
        );
    }

    @Test
    void placesAgainIfWasUnplaced(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final String binary = "some.class";
        PlaceMojoTest.saveBinary(temp, "with old content", binary);
        maven.execute(PlaceMojo.class).result();
        final String updated = "with some new content";
        PlaceMojoTest.saveBinary(temp, updated, binary);
        maven.placed().unplaceAll();
        maven.execute(PlaceMojo.class).result();
        MatcherAssert.assertThat(
            "The binary file must be replaced with new content, but it was not",
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
        new Saved(
            content,
            temp.resolve("target").resolve(ResolveMojo.DIR).resolve(
                Paths.get(String.format("%s/%s", PlaceMojoTest.LIBRARY, binary))
            )
        ).value();
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
        new Saved(
            content,
            temp.resolve(PlaceMojoTest.TARGET_CLASSES).resolve(binary)
        ).value();
    }

    /**
     * Path to the placed binary.
     * @param temp Temp test directory
     * @param binary Binary name.
     * @return Path to the placed binary.
     */
    private static Path pathToPlacedBinary(final Path temp, final String binary) {
        return temp.resolve(PlaceMojoTest.TARGET_CLASSES).resolve(binary);
    }
}
