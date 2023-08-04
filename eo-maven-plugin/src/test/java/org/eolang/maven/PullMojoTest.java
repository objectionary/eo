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

import com.yegor256.tojos.MnCsv;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.Map;
import org.cactoos.io.ResourceOf;
import org.cactoos.map.MapEntry;
import org.cactoos.text.Joined;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChCompound;
import org.eolang.maven.hash.ChPattern;
import org.eolang.maven.hash.ChText;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
import org.eolang.maven.name.ObjectName;
import org.eolang.maven.name.OnCached;
import org.eolang.maven.name.OnDefault;
import org.eolang.maven.objectionary.Objectionaries;
import org.eolang.maven.objectionary.ObjsDefault;
import org.eolang.maven.objectionary.OyRemote;
import org.eolang.maven.util.Home;
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
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
@ExtendWith(OnlineCondition.class)
final class PullMojoTest {
    /**
     * Stdout.
     */
    private static final String STDOUT = "org.eolang.io.stdout";

    /**
     * Stdout source.
     */
    private static final String SOURCE = "%s/org/eolang/io/stdout.eo";

    /**
     * Versioned source.
     */
    private static final ObjectName VERSIONED = new OnCached(
        new OnDefault("%s/org/eolang/io/stdout", new CommitHash.ChConstant("9c93528.eo"))
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
            PullMojoTest.exists(temp, PullMojoTest.SOURCE),
            Matchers.is(true)
        );
    }

    @Test
    void pullsFromProbes(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp)
            .withProgram(
                "+package org.eolang.custom",
                "",
                "[] > main",
                "  QQ.io.stdout > @",
                "    QQ.txt.sprintf \"I am %d years old\"",
                "      plus.",
                "        1337",
                "        228"
            )
            .with(
                "objectionaries",
                new Objectionaries.Fake(
                    new OyRemote(
                        new ChCompound(null, null, "master")
                    )
                )
            )
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            PullMojoTest.exists(temp, PullMojoTest.SOURCE),
            Matchers.is(true)
        );
    }

    @Test
    void pullsUsingOfflineHashFile(@TempDir final Path temp) throws IOException {
        new Home(temp).save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            Paths.get("tags.txt")
        );
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withVersion("*.*.*");
        maven.with("skip", false)
            .with(
                "hsh",
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
                "hsh",
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
            PullMojoTest.exists(temp, PullMojoTest.SOURCE),
            Matchers.is(false)
        );
    }

    @Test
    void pullsVersionedObjectSuccessfully(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).with("withVersions", true);
        maven.externalTojos()
            .add(new OnDefault(PullMojoTest.STDOUT, new CommitHash.ChConstant("9c93528")))
            .withVersion("*.*.*");
        maven.execute(PullMojo.class);
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(PullMojoTest.VERSIONED.toString())
            ),
            PullMojoTest.exists(temp, PullMojoTest.VERSIONED.toString()),
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
                PullMojoTest.path(PullMojoTest.VERSIONED.toString())
            ),
            PullMojoTest.exists(temp, PullMojoTest.VERSIONED.toString()),
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
            .with("hsh", fourth)
            .withVersionedProgram()
            .execute(new FakeMaven.Pull());
        final ObjectName sprintf = new OnCached(
            new OnDefault("%s/org/eolang/txt/sprintf", new CommitHash.ChConstant("17f8929.eo"))
        );
        final ObjectName string = new OnCached(
            new OnDefault("%s/org/eolang/string", new CommitHash.ChConstant("5f82cc1.eo"))
        );
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(PullMojoTest.VERSIONED.toString())
            ),
            PullMojoTest.exists(temp, PullMojoTest.VERSIONED.toString()),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(sprintf.toString())
            ),
            PullMojoTest.exists(temp, sprintf.toString()),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "File by path %s should have existed after pulling, but it didn't",
                PullMojoTest.path(string.toString())
            ),
            PullMojoTest.exists(temp, string.toString()),
            Matchers.is(true)
        );
    }

    /**
     * Check if the given source file exists in the target directory.
     *
     * @param temp Test temporary directory.
     * @param source Source file.
     * @return If given source file exists.
     */
    private static boolean exists(final Path temp, final String source) {
        return new Home(temp.resolve("target")).exists(
            Paths.get(PullMojoTest.path(source))
        );
    }

    /**
     * Format given a source path.
     * @param source Source path.
     * @return Formatted source path.
     */
    private static String path(final String source) {
        return String.format(source, PullMojo.DIR);
    }
}
