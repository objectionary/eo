/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.SecureRandom;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link UnplaceMojo}.
 *
 * @since 0.1
 * @checkstyle LocalFinalVariableNameCheck (100 lines)
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(MktmpResolver.class)
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
    void cleansClasses(@Mktmp final Path temp) throws IOException {
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        MatcherAssert.assertThat(
            "After executing UnplaceMojo, all class files should be removed",
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
    void cleansBinariesWithJar(@Mktmp final Path temp) throws IOException {
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeJar(temp, UnplaceMojoTest.DEFAULT_JAR);
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final List<TjPlaced> tojos = new TjsPlaced(placed).allBinaries();
        new FakeMaven(temp)
            .with("placed", placed.toFile())
            .execute(UnplaceMojo.class);
        final int expected = 5;
        MatcherAssert.assertThat(
            String.format(
                "Expected %d binaries, but it's not",
                expected
            ),
            tojos.size(),
            Matchers.equalTo(expected)
        );
        MatcherAssert.assertThat(
            "All binaries should be marked as unplaced after cleanup",
            tojos.stream().allMatch(TjPlaced::unplaced),
            Matchers.is(true)
        );
    }

    @Test
    void keepsJarBecauseItIsStillInUse(@Mktmp final Path temp) throws IOException {
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final String other = "other-jar";
        UnplaceMojoTest.placeJar(temp, other);
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final List<TjPlaced> tojos = new TjsPlaced(placed).allBinaries();
        new FakeMaven(temp)
            .with("placed", placed.toFile())
            .execute(UnplaceMojo.class);
        final int expected = 5;
        MatcherAssert.assertThat(
            String.format(
                "Expected %d binaries, but got a different number",
                expected
            ),
            tojos.size(),
            Matchers.equalTo(expected)
        );
        MatcherAssert.assertThat(
            String.format("JAR file %s should still be placed as it is in use", other),
            tojos.stream()
                .filter(tojo -> tojo.identifier().equals(other))
                .allMatch(TjPlaced::placed),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @MethodSource("testArgsProvider")
    void unplacesWithKeepOrRemoveBinariesParam(final String[] params, @Mktmp final Path temp)
        throws Exception {
        final Path placed = UnplaceMojoTest.placeClass(temp, UnplaceMojoTest.clazz(temp));
        final FakeMaven maven = new FakeMaven(temp)
            .with("placed", placed.toFile());
        for (final String param : params) {
            maven.with(param, UnplaceMojoTest.GLOB_PATTERN);
        }
        final Map<String, Path> res = maven.execute(UnplaceMojo.class).result();
        if (params.length == 1 && "keepBinaries".equals(params[0])) {
            MatcherAssert.assertThat(
                "Class files must be kept, but they were removed",
                res.values().stream().anyMatch(UnplaceMojoTest::isClass),
                Matchers.is(true)
            );
            MatcherAssert.assertThat(
                "Output must contain false, but it doesn't",
                new TextOf(res.get(placed.getFileName().toString())).asString(),
                Matchers.allOf(
                    Matchers.containsString("false"),
                    Matchers.not(Matchers.containsString("true"))
                )
            );
        } else {
            MatcherAssert.assertThat(
                "Class files must be removed, but some were kept",
                res.values().stream().noneMatch(UnplaceMojoTest::isClass),
                Matchers.is(true)
            );
            MatcherAssert.assertThat(
                "Output must contain false, but it doesn't",
                new TextOf(res.get(placed.getFileName().toString())).asString(),
                Matchers.allOf(
                    Matchers.not(Matchers.containsString("false")),
                    Matchers.containsString("true")
                )
            );
        }
    }

    @Test
    void unplacesWithRemoveBinaries(@Mktmp final Path temp) throws Exception {
        final Path target = Paths.get("target");
        final Path source = target
            .resolve("classes")
            .resolve("EOorg")
            .resolve("EOeolang")
            .resolve("EOtxt")
            .resolve("EOregexp.class");
        final Path test = target
            .resolve("test-classes")
            .resolve("EOorg")
            .resolve("EOeolang")
            .resolve("EOtxt")
            .resolve("EOregexp.class");
        final Path remaining = target
            .resolve("test-classes")
            .resolve("EOorg")
            .resolve("EOeolang")
            .resolve("EOharmcrest")
            .resolve("EOassert.class");
        new Saved(UUID.randomUUID().toString(), temp.resolve(source)).value();
        new Saved(UUID.randomUUID().toString(), temp.resolve(test)).value();
        new Saved(UUID.randomUUID().toString(), temp.resolve(remaining)).value();
        UnplaceMojoTest.placeClass(temp, temp.resolve(source));
        final Path placed = UnplaceMojoTest.placeClass(temp, temp.resolve(test));
        new FakeMaven(temp)
            .with("placed", placed.toFile())
            .with("removeBinaries", Collections.singleton("EOorg/EOeolang/EOtxt/**"))
            .execute(UnplaceMojo.class)
            .result();
        MatcherAssert.assertThat(
            String.format("Source class %s has not to be present", source),
            temp.resolve(source).toFile(),
            Matchers.not(FileMatchers.anExistingFile())
        );
        MatcherAssert.assertThat(
            String.format("Test class %s has not to be present", test),
            temp.resolve(test).toFile(),
            Matchers.not(FileMatchers.anExistingFile())
        );
        MatcherAssert.assertThat(
            String.format("Test class %s has to be present", remaining),
            temp.resolve(remaining).toFile(),
            FileMatchers.anExistingFile()
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
        new TjsPlaced(placed).placeClass(
            clazz,
            temp.relativize(clazz).toString(),
            UnplaceMojoTest.DEFAULT_JAR
        );
        return placed;
    }

    /**
     * Saves a jar into the placed tojos file.
     * @param temp Temporary directory.
     * @param name Name of the jar.
     * @return Path to the placed tojos file.
     */
    private static void placeJar(final Path temp, final String name) {
        new TjsPlaced(UnplaceMojoTest.placedFile(temp)).placeJar(name);
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
        final Path path =
            Paths.get(String.format("a/b/c/%d_foo.class", new SecureRandom().nextInt()));
        return new Saved(
            () -> UUID.randomUUID().toString(),
            temp.resolve(path)
        ).value();
    }

    /**
     * Checks if the path is a class file.
     * @param path The path to check.
     * @return True if it is a class file.
     */
    private static boolean isClass(final Path path) {
        return path.toString().endsWith(".class");
    }

    /**
     * Input arguments for unit tests.
     *
     * @return Stream of arguments.
     */
    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Stream<Arguments> testArgsProvider() {
        return Stream.of(
            Arguments.of((Object) new String[]{"keepBinaries"}),
            Arguments.of((Object) new String[]{"removeBinaries"}),
            Arguments.of((Object) new String[]{"keepBinaries", "removeBinaries"})
        );
    }
}
