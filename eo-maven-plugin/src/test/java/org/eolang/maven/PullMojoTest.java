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
import org.cactoos.io.ResourceOf;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChCompound;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.objectionary.Objectionary;
import org.eolang.maven.objectionary.OyRemote;
import org.eolang.maven.objectionary.OysSimple;
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

    @Test
    void pullsSuccessfully(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add("org.eolang.io.stdout")
            .withVersion("*.*.*");
        maven.with("objectionary", new Objectionary.Fake())
            .with("skip", false)
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            new Home(temp.resolve("target")).exists(
                Paths.get(
                    String.format(
                        "%s/org/eolang/io/stdout.eo",
                        PullMojo.DIR
                    )
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(OnlineCondition.class)
    void pullsFromProbes(@TempDir final Path temp) throws IOException {
        final CommitHash hash = new ChCached(
            new ChCompound(null, null, "master")
        );
        final Objectionary objectionary = new OyRemote(hash);
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
            .with("objectionary", objectionary)
            .with("objectionaries", new OysSimple().with(hash, objectionary))
            .with("defaultHash", hash)
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            new Home(temp.resolve("target")).exists(
                Paths.get(
                    String.format(
                        "%s/org/eolang/io/stdout.eo",
                        PullMojo.DIR
                    )
                )
            ),
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
            .add("org.eolang.io.stdout")
            .withVersion("*.*.*");
        maven.with("objectionary", new Objectionary.Fake())
            .with("offlineHashFile", temp.resolve("tags.txt"))
            .with("skip", false)
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
            .add("org.eolang.io.stdout")
            .withVersion("*.*.*");
        maven.with("objectionary", new Objectionary.Fake())
            .with("tag", "1.0.0")
            .with("offlineHash", "*.*.*:abcdefg")
            .with("skip", false)
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
            .add("org.eolang.io.stdout")
            .withScope("compile")
            .withVersion("*.*.*");
        maven.with("skip", true)
            .with("objectionary", new Objectionary.Fake())
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            new Home(temp.resolve("target")).exists(
                Paths.get(
                    String.format(
                        "%s/org/eolang/io/stdout.eo",
                        PullMojo.DIR
                    )
                )
            ),
            Matchers.is(false)
        );
    }
}
