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
import com.yegor256.tojos.MnCsv;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.Map;
import org.cactoos.io.ResourceOf;
import org.cactoos.map.MapEntry;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChPattern;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.ChText;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
import org.eolang.maven.name.ObjectName;
import org.eolang.maven.name.OnVersioned;
import org.eolang.maven.objectionary.Objectionaries;
import org.eolang.maven.objectionary.ObjsDefault;
import org.eolang.maven.objectionary.OyRemote;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link PullMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(WeAreOnline.class)
final class PullMojoTest {
    /**
     * Stdout.
     */
    private static final String STDOUT = "org.eolang.io.stdout";

    /**
     * Versioned source.
     */
    private static final ObjectName VERSIONED = new OnVersioned(
        "org.eolang.io.stdout",
        "9c93528"
    );

    @Test
    void pullsSuccessfully(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withVersion("*.*.*");
        maven.with("skip", false)
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            PullMojoTest.exists(temp, PullMojoTest.STDOUT),
            Matchers.is(true)
        );
    }

    @Test
    void pullsFromProbes(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp)
            .withProgram(
                "+package org.eolang.custom\n",
                "# This is the default 64+ symbols comment in front of named abstract object.",
                "[] > main",
                "  QQ.io.stdout > @",
                "    QQ.txt.sprintf",
                "      \"I am %d years old\"",
                "      plus.",
                "        1337",
                "        228"
            )
            .with(
                "objectionaries",
                new Objectionaries.Fake(
                    new OyRemote(
                        new ChRemote("master")
                    )
                )
            )
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            PullMojoTest.exists(temp, PullMojoTest.STDOUT),
            Matchers.is(true)
        );
    }

    @Test
    void pullsUsingOfflineHashFile(@TempDir final Path temp) throws IOException {
        new HmBase(temp).save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            Paths.get("tags.txt")
        );
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withVersion("*.*.*");
        maven.with("skip", false)
            .with(
                "hash",
                new ChCached(new ChText(temp.resolve("tags.txt"), "master"))
            )
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            new LinkedList<>(new MnCsv(maven.foreignPath()).read()).getFirst().get("hash"),
            Matchers.equalTo("mmmmmmm")
        );
    }

    /**
     * Offline hash test.
     *
     * @param temp Temporary directory for test.
     */
    @Test
    void pullsUsingOfflineHash(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withVersion("*.*.*");
        maven.with("skip", false)
            .with(
                "hash",
                new ChCached(new ChPattern("*.*.*:abcdefg", "1.0.0"))
            )
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            new LinkedList<>(new MnCsv(maven.foreignPath()).read()).getFirst().get("hash"),
            Matchers.equalTo("abcdefg")
        );
    }

    @Test
    void skipsPullMojo(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withScope("compile")
            .withVersion("*.*.*");
        maven.with("skip", true)
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            PullMojoTest.exists(temp, PullMojoTest.STDOUT),
            Matchers.is(false)
        );
    }

    @Test
    void pullsVersionedObjectSuccessfully(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(new OnVersioned(PullMojoTest.STDOUT, "9c93528"))
            .withVersion("*.*.*");
        maven.with("withVersions", true)
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(PullMojoTest.VERSIONED)
            ),
            PullMojoTest.exists(temp, PullMojoTest.VERSIONED),
            Matchers.is(true)
        );
    }

    @Test
    void pullsProbedVersionedObjectFromOneObjectionary(@TempDir final Path temp)
        throws IOException {
        new FakeMaven(temp)
            .with("withVersions", true)
            .with(
                "objectionaries",
                new Objectionaries.Fake(
                    new OyRemote(
                        new ChCached(new CommitHashesMap.Fake().get("0.28.5"))
                    )
                )
            )
            .withVersionedHelloWorld()
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(PullMojoTest.VERSIONED)
            ),
            PullMojoTest.exists(temp, PullMojoTest.VERSIONED),
            Matchers.is(true)
        );
    }

    @Test
    void pullsProbedVersionedObjectsFromDifferentObjectionaries(@TempDir final Path temp)
        throws IOException {
        final Map<String, CommitHash> hashes = new CommitHashesMap.Fake();
        final CommitHash first = hashes.get("0.28.4");
        final CommitHash second = hashes.get("0.28.5");
        final CommitHash third = hashes.get("0.28.6");
        final CommitHash fourth = hashes.get("0.28.7");
        new FakeMaven(temp)
            .with(
                "objectionaries",
                new ObjsDefault(
                    new MapEntry<>(first, new OyRemote(first)),
                    new MapEntry<>(second, new OyRemote(second)),
                    new MapEntry<>(third, new OyRemote(third)),
                    new MapEntry<>(fourth, new OyRemote(fourth))
                )
            )
            .with("withVersions", true)
            .with("hash", fourth)
            .withVersionedProgram()
            .execute(new FakeMaven.Pull());
        final ObjectName sprintf = new OnVersioned("org.eolang.txt.sprintf", "17f8929");
        final ObjectName string = new OnVersioned("org.eolang.string", "5f82cc1");
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(PullMojoTest.VERSIONED)
            ),
            PullMojoTest.exists(temp, PullMojoTest.VERSIONED),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(sprintf)
            ),
            PullMojoTest.exists(temp, sprintf),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(string)
            ),
            PullMojoTest.exists(temp, string),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotPullInOfflineMode(@TempDir final Path tmp) throws IOException {
        final Map<String, Path> result = new FakeMaven(tmp)
            .withHelloWorld()
            .with("offline", true)
            .execute(new FakeMaven.Pull())
            .result();
        final String format = "%s folder should not contain %s file, but it did";
        final String stdout = "org/eolang/io/stdout.eo";
        final String string = "org/eolang/string.eo";
        MatcherAssert.assertThat(
            String.format(format, PullMojo.DIR, stdout),
            result.containsKey(String.format("%s/%s", PullMojo.DIR, stdout)),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            String.format(format, PullMojo.DIR, string),
            result.containsKey(String.format("%s/%s", PullMojo.DIR, string)),
            Matchers.is(false)
        );
    }

    /**
     * Check if the given source file exists in the target directory.
     * @param temp Test temporary directory.
     * @param source Source file as object name.
     * @return If given source file exists.
     */
    private static boolean exists(final Path temp, final ObjectName source) {
        return PullMojoTest.exists(temp, source.toString());
    }

    /**
     * Check if the given source file exists in the target directory.
     *
     * @param temp Test temporary directory.
     * @param source Source file.
     * @return If given source file exists.
     */
    private static boolean exists(final Path temp, final String source) {
        return new HmBase(temp.resolve("target")).exists(PullMojoTest.path(source));
    }

    /**
     * Format given a source path.
     * @param name Source path as object name.
     * @return Formatted source path.
     */
    private static Path path(final ObjectName name) {
        return PullMojoTest.path(name.toString());
    }

    /**
     * Format given a source path.
     * @param source Source path as object name.
     * @return Formatted source path.
     */
    private static Path path(final String source) {
        return new Place(source).make(Paths.get(PullMojo.DIR), "eo");
    }

}
