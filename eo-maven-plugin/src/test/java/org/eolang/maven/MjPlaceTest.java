/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
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
 * Test case for {@link MjPlace}.
 *
 * @since 0.11
 */
@SuppressWarnings("PMD.TooManyMethods")
@ExtendWith(MktmpResolver.class)
final class MjPlaceTest {

    @Test
    void placesBinaries(@Mktmp final Path temp) throws Exception {
        MjPlaceTest.saveBinary(temp, "EObar/x.bin");
        MjPlaceTest.saveBinary(temp, "org/eolang/f/x.a.class");
        MjPlaceTest.saveBinary(temp, "org/eolang/t.txt");
        MatcherAssert.assertThat(
            "PlaceMojo have to place binaries, but it doesn't",
            new FakeMaven(temp)
                .execute(MjPlace.class)
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
        MjPlaceTest.saveBinary(temp, binary);
        MjPlaceTest.saveAlreadyPlacedBinary(temp, binary);
        final long before = MjPlaceTest.pathToPlacedBinary(
            temp,
            binary
        ).toFile().lastModified();
        MatcherAssert.assertThat(
            "PlaceMojo must skip already placed binaries, but it doesn't",
            new FakeMaven(temp)
                .withPlacedBinary(
                    temp.resolve(this.targetClasses()).resolve(binary)
                )
            .execute(MjPlace.class)
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
        MjPlaceTest.saveBinary(temp, content, binary);
        MjPlaceTest.saveAlreadyPlacedBinary(temp, "old content", binary);
        final Path path = MjPlaceTest.pathToPlacedBinary(temp, binary);
        final FakeMaven maven = new FakeMaven(temp).withPlacedBinary(path);
        maven.placed().unplaceAll();
        MatcherAssert.assertThat(
            "PlaceMojo have to process the file",
            maven.execute(MjPlace.class).result(),
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
        Files.createDirectories(temp.resolve("target").resolve(MjResolve.DIR));
        MatcherAssert.assertThat(
            String.format(
                "PlaceMojo must not place binaries from %s",
                this.targetClasses()
            ),
            new FakeMaven(temp)
                .execute(MjPlace.class)
                .result(),
            Matchers.not(Matchers.hasKey(this.targetClasses()))
        );
    }

    @Test
    void placesDefaultJnaBinaries(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "PlaceMojo have to place default Jna binary",
            new FakeMaven(temp)
                .withHelloWorld()
                .with("ignoreRuntime", true)
                .execute(new FakeMaven.Place())
                .result()
                .get(this.targetClasses()),
            new ContainsFiles("**/jna-*.class")
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
                this.targetClasses()
            ),
            new FakeMaven(temp)
                .execute(MjPlace.class)
                .result(),
            Matchers.not(Matchers.hasKey(this.targetClasses()))
        );
    }

    @Test
    void placesMissing(@Mktmp final Path temp) throws IOException {
        final String first = "EObar/x.bin";
        final String second = "org/eolang/f/x.a.class";
        MjPlaceTest.saveBinary(temp, first);
        MjPlaceTest.saveBinary(temp, second);
        MjPlaceTest.saveAlreadyPlacedBinary(temp, first);
        MatcherAssert.assertThat(
            "The first binary file must be placed, but it was not",
            new FakeMaven(temp)
                .execute(MjPlace.class)
                .result(),
            Matchers.allOf(
                Matchers.hasValue(MjPlaceTest.pathToPlacedBinary(temp, first)),
                Matchers.hasValue(MjPlaceTest.pathToPlacedBinary(temp, second))
            )
        );
    }

    /**
     * Test case for {@link MjPlace#execute()}.
     * Since for tests we are using dummy maven central, then instead of unpacking
     * of classes from jar it just copies the just simple .class files to target/classes folder.
     *
     * @param temp Temporary directory
     * @throws IOException If fails
     */
    @Test
    void placesAllEoRuntimeClasses(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            "PlaceMojo have to place the runtime file, but doesn't",
            maven.withHelloWorld()
                .with("resolveJna", false)
                .execute(new FakeMaven.Place())
                .result()
                .get(this.targetClasses()),
            new ContainsFiles("**/eo-runtime-*.class")
        );
        MatcherAssert.assertThat(
            "PlaceMojo have to place class file, but doesn't",
            maven.placed().classes().size(),
            Matchers.is(1)
        );
    }

    @Test
    void placesWithoutEoRuntimeClasses(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            "PlaceMojo have not to place the runtime file, but doesn't",
            maven.withHelloWorld()
                .with("ignoreRuntime", true)
                .with("resolveJna", false)
                .execute(new FakeMaven.Place())
                .result()
                .get(this.targetClasses()),
            Matchers.not(new ContainsFiles("**/eo-runtime-*.class"))
        );
    }

    @Test
    void doesNotPlacesAgainIfWasNotUnplaced(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final String binary = "some.class";
        final String old = "some old content";
        MjPlaceTest.saveBinary(temp, old, binary);
        maven.execute(MjPlace.class).result();
        MjPlaceTest.saveBinary(temp, "new content", binary);
        maven.execute(MjPlace.class).result();
        MatcherAssert.assertThat(
            "The binary file must not be replaced with new content, but it was not",
            new TextOf(MjPlaceTest.pathToPlacedBinary(temp, binary)).asString(),
            Matchers.equalTo(old)
        );
    }

    @Test
    void placesAgainIfWasUnplaced(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final String binary = "some.class";
        MjPlaceTest.saveBinary(temp, "with old content", binary);
        maven.execute(MjPlace.class).result();
        final String updated = "with some new content";
        MjPlaceTest.saveBinary(temp, updated, binary);
        maven.placed().unplaceAll();
        maven.execute(MjPlace.class).result();
        MatcherAssert.assertThat(
            "The binary file must be replaced with new content, but it was not",
            new TextOf(MjPlaceTest.pathToPlacedBinary(temp, binary)).asString(),
            Matchers.equalTo(updated)
        );
    }

    /**
     * Returns the target classes.
     */
    private String targetClasses() {
        return "target/classes";
    }

    /**
     * Save binary to {@link MjResolve#DIR} folder.
     * The method emulates the situation when we have some resolved binaries.
     *
     * @param temp   Temp test directory.
     * @param binary Binary name.
     * @throws IOException In case of error.
     */
    private static void saveBinary(final Path temp, final String binary) throws IOException {
        MjPlaceTest.saveBinary(temp, UUID.randomUUID().toString(), binary);
    }

    /**
     * Save binary to {@link MjResolve#DIR} folder.
     * The method emulates the situation when we have some resolved binaries.
     *
     * @param temp    Temp test directory.
     * @param content Content of the binary.
     * @param binary  Binary name.
     * @throws IOException In case of error.
     */
    private static void saveBinary(
        final Path temp,
        final String content,
        final String binary
    ) throws IOException {
        new Saved(
            content,
            temp.resolve("target").resolve(MjResolve.DIR).resolve(
                Paths.get(String.format("%s/%s", "foo/hello/-/0.1", binary))
            )
        ).value();
    }

    /**
     * Save binary to classes folder.
     * The method emulates the situation when we already have some placed binaries.
     *
     * @param temp   Temp test directory.
     * @param binary Binary name.
     * @throws IOException In case of error.
     */
    private static void saveAlreadyPlacedBinary(
        final Path temp,
        final String binary
    ) throws IOException {
        MjPlaceTest.saveAlreadyPlacedBinary(temp, UUID.randomUUID().toString(), binary);
    }

    /**
     * Save binary to classes folder.
     * The method emulates the situation when we already have some placed binaries.
     *
     * @param temp    Temp test directory.
     * @param content Content of the binary.
     * @param binary  Binary name.
     * @throws IOException In case of error.
     */
    private static void saveAlreadyPlacedBinary(
        final Path temp,
        final String content,
        final String binary
    ) throws IOException {
        final String targets = "target/classes";
        new Saved(
            content,
            temp.resolve(targets).resolve(binary)
        ).value();
    }

    /**
     * Path to the placed binary.
     *
     * @param temp   Temp test directory
     * @param binary Binary name.
     * @return Path to the placed binary.
     */
    private static Path pathToPlacedBinary(final Path temp, final String binary) {
        return temp.resolve("target/classes").resolve(binary);
    }
}
