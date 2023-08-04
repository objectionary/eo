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
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChPattern;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.ChText;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
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
 * Test case for {@link ProbeMojo}.
 *
 * @since 0.28.11
 * @todo #2302:30min Refactor tests in the class. Looks like there is a lot of
 *  code duplication among all tests in the class. Need to reduce it somehow.
 * @todo #2302:30min Refactor firstEntity method. The "first entity of the
 *  foreign tojos" looks strange. It looks like we are tying to scan an
 *  intermediate state which we are trying to read in the middle of the process.
 *  It lead to a fragile implementation. Could we check the result somehow else?
 *  Or to skip that check at all?
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
@ExtendWith(OnlineCondition.class)
final class ProbeMojoTest {
    /**
     * Stdout.
     */
    private static final String STDOUT = "org.eolang.io.stdout|9c93528";

    @Test
    @ExtendWith(OnlineCondition.class)
    void findsProbes(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            ProbeMojoTest.firstEntry(
                new FakeMaven(temp)
                    .with("foreignFormat", "json")
                    .withProgram(ProbeMojoTest.program())
                    .execute(new FakeMaven.Probe())
                    .foreignPath(),
                "probed"
            ),
            Matchers.equalTo("5")
        );
    }

    @Test
    void findsProbesViaOfflineHashFile(@TempDir final Path temp) throws IOException {
        new Home(temp).save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            Paths.get("tags.txt")
        );
        final CommitHash hash = new ChCached(
            new ChText(temp.resolve("tags.txt"), "master")
        );
        MatcherAssert.assertThat(
            ProbeMojoTest.firstEntry(
                new FakeMaven(temp)
                    .with("hsh", hash)
                    .withProgram(ProbeMojoTest.program())
                    .execute(new FakeMaven.Probe())
                    .foreignPath(),
                "hash"
            ),
            Matchers.equalTo("mmmmmmm")
        );
    }

    @Test
    void findsProbesViaOfflineHash(@TempDir final Path temp) throws IOException {
        final CommitHash hash = new ChCached(
            new ChPattern("*.*.*:abcdefg", "1.0.0")
        );
        MatcherAssert.assertThat(
            ProbeMojoTest.firstEntry(
                new FakeMaven(temp)
                    .with("hsh", hash)
                    .withProgram(ProbeMojoTest.program())
                    .execute(new FakeMaven.Probe())
                    .foreignPath(),
                "hash"
            ),
            Matchers.equalTo("abcdefg")
        );
    }

    @Test
    @ExtendWith(OnlineCondition.class)
    void findsProbesInOyRemote(@TempDir final Path temp) throws IOException {
        final String tag = "0.28.10";
        final CommitHash hash = new ChRemote(tag);
        MatcherAssert.assertThat(
            ProbeMojoTest.firstEntry(
                new FakeMaven(temp)
                    .with("tag", tag)
                    .with("objectionaries", new Objectionaries.Fake(new OyRemote(hash)))
                    .withProgram(ProbeMojoTest.program())
                    .execute(new FakeMaven.Probe())
                    .foreignPath(),
                "probed"
            ),
            Matchers.equalTo("2")
        );
    }

    @Test
    @ExtendWith(OnlineCondition.class)
    void findsProbesWithVersionsInOneObjectionary(@TempDir final Path temp) throws IOException {
        final CommitHash hash = new CommitHashesMap.Fake().get("0.28.5");
        final FakeMaven maven = new FakeMaven(temp)
            .with("hsh", hash)
            .with("objectionaries", new Objectionaries.Fake(new OyRemote(hash)))
            .with("withVersions", true)
            .withVersionedHelloWorld()
            .execute(new FakeMaven.Probe());
        MatcherAssert.assertThat(
            String.format(
                "Tojos should have contained versioned object %s after probing, but they didn't",
                ProbeMojoTest.STDOUT
            ),
            maven.externalTojos().contains(ProbeMojoTest.STDOUT),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "First entry of tojos after probing should have contained one probed object, but it didn't",
            ProbeMojoTest.firstEntry(
                maven.externalPath(),
                "probed"
            ),
            Matchers.equalTo("1")
        );
        MatcherAssert.assertThat(
            "First entry of tojos after probing should have contained given hash, but it didn't",
            ProbeMojoTest.firstEntry(
                maven.externalPath(),
                "hash"
            ),
            Matchers.equalTo(hash.value())
        );
    }

    @Test
    @ExtendWith(OnlineCondition.class)
    void findsProbesWithVersionsInDifferentObjectionaries(@TempDir final Path temp)
        throws IOException {
        final Map<String, CommitHash> hashes = new CommitHashesMap.Fake();
        final CommitHash first = hashes.get("0.28.5");
        final CommitHash second = hashes.get("0.28.6");
        final String number = "org.eolang.txt.text|5f82cc1";
        final FakeMaven maven = new FakeMaven(temp)
            .with(
                "objectionaries",
                new ObjsDefault(
                    new MapEntry<>(first, new OyRemote(first)),
                    new MapEntry<>(second, new OyRemote(second))
                )
            )
            .with("withVersions", true)
            .with("hsh", first)
            .withVersionedProgram()
            .execute(new FakeMaven.Probe());
        MatcherAssert.assertThat(
            String.format(
                "Tojos should have contained versioned object %s after probing, but they didn't",
                ProbeMojoTest.STDOUT
            ),
            maven.externalTojos().contains(ProbeMojoTest.STDOUT),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "Tojos should have contained versioned object %s after probing, but they didn't",
                number
            ),
            maven.externalTojos().contains(number),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "First entry of tojos after probing should have contained two probed objects, but it didn't",
            ProbeMojoTest.firstEntry(
                maven.externalPath(),
                "probed"
            ),
            Matchers.equalTo("2")
        );
        MatcherAssert.assertThat(
            "First entry of tojos after probing should have contained given hash, but it didn't",
            ProbeMojoTest.firstEntry(
                maven.externalPath(),
                "hash"
            ),
            Matchers.equalTo(first.value())
        );
    }

    private static String program() {
        return new UncheckedText(
            new TextOf(
                new ResourceOf("org/eolang/maven/simple-io.eo")
            )
        ).asString();
    }

    private static String firstEntry(final Path foreign, final String field) {
        return new LinkedList<>(new MnCsv(foreign.toFile()).read()).getFirst().get(field);
    }
}
