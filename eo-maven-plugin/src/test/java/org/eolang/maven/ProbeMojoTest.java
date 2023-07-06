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
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.objectionary.Objectionary;
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
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
@ExtendWith(OnlineCondition.class)
final class ProbeMojoTest {

    @Test
    void findsProbes(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            ProbeMojoTest.firstEntry(
                new FakeMaven(temp)
                    .with("foreignFormat", "json")
                    .with("objectionary", new Objectionary.Fake())
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
        MatcherAssert.assertThat(
            ProbeMojoTest.firstEntry(
                new FakeMaven(temp)
                    .with("offlineHashFile", temp.resolve("tags.txt"))
                    .with("objectionary", new Objectionary.Fake())
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
        MatcherAssert.assertThat(
            ProbeMojoTest.firstEntry(
                new FakeMaven(temp)
                    .with("tag", "1.0.0")
                    .with("offlineHash", "*.*.*:abcdefg")
                    .with("objectionary", new Objectionary.Fake())
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
        MatcherAssert.assertThat(
            ProbeMojoTest.firstEntry(
                new FakeMaven(temp)
                    .with("tag", tag)
                    .with("objectionary", new OyRemote(new ChCached(new ChRemote(tag))))
                    .withProgram(ProbeMojoTest.program())
                    .execute(new FakeMaven.Probe())
                    .foreignPath(),
                "probed"
            ),
            Matchers.equalTo("2")
        );
    }

    @Test
    void comparesForeignAndExternalTojosAfterProbing(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("objectionary", new Objectionary.Fake())
            .withProgram(ProbeMojoTest.program())
            .execute(new FakeMaven.Probe());
        MatcherAssert.assertThat(
            maven.foreignTojos().status(),
            Matchers.equalTo(maven.externalTojos().status())
        );
    }

    @Test
    void comparesForeignAndExternalTojosAfterProbingManyPrograms(
        @TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("objectionary", new Objectionary.Fake());
        final int count = 20;
        for (int program = 0; program < count; ++program) {
            maven.withProgram(ProbeMojoTest.program());
        }
        maven.execute(new FakeMaven.Probe());
        MatcherAssert.assertThat(
            maven.foreignTojos().status(),
            Matchers.equalTo(maven.externalTojos().status())
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
