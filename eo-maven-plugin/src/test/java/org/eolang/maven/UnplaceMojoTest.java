/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
import org.eolang.maven.tojos.PlacedTojo;
import org.eolang.maven.tojos.PlacedTojos;
import org.eolang.maven.util.HmBase;
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
            CatalogsTest.TO_ADD_MESSAGE,
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
        final List<PlacedTojo> tojos = new PlacedTojos(placed).allBinaries();
        new FakeMaven(temp)
            .with("placed", placed.toFile())
            .execute(UnplaceMojo.class);
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            tojos.size(),
            Matchers.equalTo(5)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            tojos.stream().allMatch(PlacedTojo::unplaced),
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
        final List<PlacedTojo> tojos = new PlacedTojos(placed).allBinaries();
        new FakeMaven(temp)
            .with("placed", placed.toFile())
            .execute(UnplaceMojo.class);
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            tojos.size(),
            Matchers.equalTo(5)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            tojos.stream()
                .filter(tojo -> tojo.identifier().equals(other))
                .allMatch(PlacedTojo::placed),
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
                CatalogsTest.TO_ADD_MESSAGE,
                res.values().stream().anyMatch(UnplaceMojoTest::isClass),
                Matchers.is(true)
            );
            MatcherAssert.assertThat(
                CatalogsTest.TO_ADD_MESSAGE,
                new TextOf(res.get(placed.getFileName().toString())).asString(),
                Matchers.allOf(
                    Matchers.containsString("false"),
                    Matchers.not(Matchers.containsString("true"))
                )
            );
        } else {
            MatcherAssert.assertThat(
                CatalogsTest.TO_ADD_MESSAGE,
                res.values().stream().noneMatch(UnplaceMojoTest::isClass),
                Matchers.is(true)
            );
            MatcherAssert.assertThat(
                CatalogsTest.TO_ADD_MESSAGE,
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
        final HmBase workspace = new HmBase(temp);
        workspace.save(UUID.randomUUID().toString(), source);
        workspace.save(UUID.randomUUID().toString(), test);
        workspace.save(UUID.randomUUID().toString(), remaining);
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
        new PlacedTojos(placed).placeClass(
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
        new PlacedTojos(UnplaceMojoTest.placedFile(temp)).placeJar(name);
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
        new HmBase(temp).save(
            () -> UUID.randomUUID().toString(),
            path
        );
        return temp.resolve(path);
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

