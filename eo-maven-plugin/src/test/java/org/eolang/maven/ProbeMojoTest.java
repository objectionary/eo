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
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
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
 * Test case for {@link ProbeMojo}.
 *
 * @since 0.28.11
 */
@ExtendWith(WeAreOnline.class)
final class ProbeMojoTest {
    /**
     * Stdout.
     */
    private static final ObjectName STDOUT = new OnVersioned("org.eolang.io.stdout", "9c93528");

    @Test
    @ExtendWith(WeAreOnline.class)
    void findsProbes(@TempDir final Path temp) throws Exception {
        final String expected = "5";
        MatcherAssert.assertThat(
            String.format(
                "Number of objects that we should find during the probing phase should be equal %s",
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
        final String tag = "master";
        final String tags = "org/eolang/maven/commits/tags.txt";
        new HmBase(temp).save(
            new ResourceOf(tags),
            Paths.get("tags.txt")
        );
        MatcherAssert.assertThat(
            String.format(
                "The hash of the program should be equal to the hash of the commit for the '%s' tag. See '%s' file",
                tag,
                tags
            ),
            new FakeMaven(temp)
                .with(
                    "hash",
                    new ChCached(
                        new ChText(temp.resolve("tags.txt"), tag)
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
            "The hash of the program tojo should be equal to the given hash pattern",
            new FakeMaven(temp)
                .with("hash", new ChPattern("*.*.*:abcdefg", "1.0.0"))
                .withProgram(ProbeMojoTest.program())
                .execute(new FakeMaven.Probe())
                .programTojo()
                .hash(),
            Matchers.equalTo("abcdefg")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void findsProbesInOyRemote(@TempDir final Path temp) throws IOException {
        final String tag = "0.28.10";
        MatcherAssert.assertThat(
            String.format(
                "The hash of the program tojo should be equal to the hash of the commit for the '%s' tag",
                tag
            ),
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
    @ExtendWith(WeAreOnline.class)
    void findsProbesWithVersionsInOneObjectionary(@TempDir final Path temp) throws IOException {
        final CommitHash hash = new CommitHashesMap.Fake().get("0.28.5");
        final FakeMaven maven = new FakeMaven(temp)
            .with("hash", hash)
            .with("objectionaries", new Objectionaries.Fake(new OyRemote(hash)))
            .with("withVersions", true)
            .withVersionedHelloWorld()
            .execute(new FakeMaven.Probe());
        MatcherAssert.assertThat(
            String.format(
                "Tojos should contain versioned object %s after probing, but they didn't",
                ProbeMojoTest.STDOUT
            ),
            maven.foreignTojos().contains(ProbeMojoTest.STDOUT),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "Program tojo entry in tojos after probing should contain one probed object",
            maven.programTojo().probed(),
            Matchers.equalTo("1")
        );
        MatcherAssert.assertThat(
            "Program tojo entry in tojos after probing should contain given hash",
            maven.programTojo().hash(),
            Matchers.equalTo(hash.value())
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void findsProbesWithVersionsInDifferentObjectionaries(@TempDir final Path temp)
        throws IOException {
        final Map<String, CommitHash> hashes = new CommitHashesMap.Fake();
        final CommitHash first = hashes.get("0.28.5");
        final CommitHash second = hashes.get("0.28.6");
        final FakeMaven maven = new FakeMaven(temp)
            .with(
                "objectionaries",
                new ObjsDefault(
                    new MapEntry<>(first, new OyRemote(first)),
                    new MapEntry<>(second, new OyRemote(second))
                )
            )
            .with("withVersions", true)
            .with("hash", first)
            .withVersionedProgram()
            .execute(new FakeMaven.Probe());
        final ObjectName text = new OnVersioned("org.eolang.txt.text", "5f82cc1");
        MatcherAssert.assertThat(
            String.format(
                "Tojos should contain versioned objects '%s' after probing, but they didn't",
                Arrays.asList(text, ProbeMojoTest.STDOUT)
            ),
            maven.foreignTojos().contains(text, ProbeMojoTest.STDOUT),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "Program tojo after probing should contain exactly two probed objects",
            maven.programTojo().probed(),
            Matchers.equalTo("2")
        );
        MatcherAssert.assertThat(
            "Program tojo after probing should have given hash",
            maven.programTojo().hash(),
            Matchers.equalTo(first.value())
        );
    }

    private static String[] program() {
        return new String[]{
            "+package org.eolang.custom\n",
            "# This is the default 64+ symbols comment in front of named abstract object.",
            "[] > main",
            "  QQ.io.stdout > @",
            "    QQ.txt.sprintf",
            "      \"I am %d years old\"",
            "      plus.",
            "        1337",
            "        228",
        };
    }
}
