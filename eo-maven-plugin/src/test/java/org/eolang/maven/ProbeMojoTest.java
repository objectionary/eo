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
import java.nio.file.Path;
import java.nio.file.Paths;
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
import org.eolang.maven.name.ObjectName;
import org.eolang.maven.name.OnVersioned;
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
 */
@ExtendWith(OnlineCondition.class)
final class ProbeMojoTest {
    /**
     * Stdout.
     */
    private static final ObjectName STDOUT = new OnVersioned("org.eolang.io.stdout", "9c93528");

    @Test
    @ExtendWith(OnlineCondition.class)
    void findsProbes(@TempDir final Path temp) throws Exception {
        final String expected = "5";
        MatcherAssert.assertThat(
            String.format(
                "Number of objects that we have found during the probing phase should be equal %s",
                expected
            ),
            new FakeMaven(temp)
                .with("foreignFormat", "json")
                .withProgram(ProbeMojoTest.program())
                .execute(new FakeMaven.Probe())
                .programTojo()
                .probed(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void findsProbesViaOfflineHashFile(@TempDir final Path temp) throws IOException {
        new Home(temp).save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            Paths.get("tags.txt")
        );
        MatcherAssert.assertThat(
            "",
            new FakeMaven(temp)
                .with(
                    "hsh",
                    new ChCached(
                        new ChText(temp.resolve("tags.txt"), "master")
                    )
                )
                .withProgram(ProbeMojoTest.program())
                .execute(new FakeMaven.Probe())
                .programTojo()
                .hash(),
            Matchers.equalTo("mmmmmmm")
        );
    }

    @Test
    void findsProbesViaOfflineHash(@TempDir final Path temp) throws IOException {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .with("hsh", new ChPattern("*.*.*:abcdefg", "1.0.0"))
                .withProgram(ProbeMojoTest.program())
                .execute(new FakeMaven.Probe())
                .programTojo()
                .hash(),
            Matchers.equalTo("abcdefg")
        );
    }

    @Test
    @ExtendWith(OnlineCondition.class)
    void findsProbesInOyRemote(@TempDir final Path temp) throws IOException {
        final String tag = "0.28.10";
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .with("tag", tag)
                .with(
                    "objectionaries",
                    new Objectionaries.Fake(new OyRemote(new ChRemote(tag)))
                )
                .withProgram(ProbeMojoTest.program())
                .execute(new FakeMaven.Probe())
                .programTojo()
                .probed(),
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
            "Program entry in tojos after probing should contain one probed object",
            maven.programExternalTojo().probed(),
            Matchers.equalTo("1")
        );
        MatcherAssert.assertThat(
            "Program entry in tojos after probing should contain given hash",
            maven.programExternalTojo().hash(),
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
        final ObjectName text = new OnVersioned("org.eolang.txt.text", "5f82cc1");
        final FakeMaven maven = new FakeMaven(temp)
            .with(
                "objectionaries",
                new ObjsDefault(
                    new MapEntry<>(first, new OyRemote(first)),
                    new MapEntry<>(second, new OyRemote(second))
                )
            )
            .with("hashes", hashes)
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
                text
            ),
            maven.externalTojos().contains(text),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "First entry of tojos after probing should have contained two probed objects, but it didn't",
            maven.programExternalTojo().probed(),
            Matchers.equalTo("2")
        );
        MatcherAssert.assertThat(
            "First entry of tojos after probing should have contained given hash, but it didn't",
            maven.programExternalTojo().hash(),
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
}
