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

import com.yegor256.tojos.MnCsv;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
import org.eolang.maven.name.ObjectName;
import org.eolang.maven.name.OnDefault;
import org.eolang.maven.name.OnVersioned;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.tojos.ForeignTojos;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link DiscoverMojo}.
 *
 * @since 0.28.11
 */
final class DiscoverMojoTest {
    /**
     * Text.
     */
    private static final ObjectName TEXT = new OnVersioned("org.eolang.txt.text", "5f82cc1");

    /**
     * Default assertion message.
     */
    private static final String SHOULD_CONTAIN =
        "Tojos should contain %s object after discovering, but they didn't";

    /**
     * Default assertion message.
     */
    private static final String SHOULD_NOT =
        "Tojos should not contain %s object after discovering, but they did";

    @ParameterizedTest
    @CsvSource({
        "org/eolang/maven/mess.eo, 8",
        "org/eolang/maven/sum.eo, 0",
        "org/eolang/maven/withwarning.eo, 1"
    })
    void executesDiscoveryPhaseForCorrectEoPrograms(
        final String program,
        final int dependencies,
        @TempDir final Path tmp
    ) throws IOException {
        final FakeMaven maven = new FakeMaven(tmp);
        maven.withProgram(new ResourceOf(program))
            .execute(new FakeMaven.Discover());
        final Deque<Map<String, String>> json = new LinkedList<>(
            new MnCsv(maven.foreignPath()).read()
        );
        final Map<String, String> first = json.removeFirst();
        MatcherAssert.assertThat(dependencies, Matchers.equalTo(json.size()));
        MatcherAssert.assertThat(
            String.valueOf(dependencies),
            Matchers.equalTo(first.get("discovered"))
        );
    }

    @Test
    void discoversForDifferentScopes(@TempDir final Path tmp) throws IOException {
        final FakeMaven maven = new FakeMaven(tmp);
        final String scope = "test";
        maven.with("scope", scope)
            .withHelloWorld()
            .execute(new FakeMaven.Discover());
        final List<String> scopes = maven.foreignTojos().all()
            .stream()
            .map(ForeignTojo::scope)
            .collect(Collectors.toList());
        MatcherAssert.assertThat(
            String.format("All tojos have the same scope '%s', but was '%s'", scope, scopes),
            scopes.stream().allMatch(s -> s.equals(scope)),
            Matchers.is(true)
        );
    }

    @Test
    void discoversWithVersions(@TempDir final Path tmp) throws IOException {
        final FakeMaven maven = new FakeMaven(tmp)
            .with("withVersions", true)
            .withVersionedProgram()
            .execute(new FakeMaven.Discover());
        final ObjectName stdout = new OnVersioned("org.eolang.stdout", "9c93528");
        final String nop = "org.eolang.nop";
        final ForeignTojos tojos = maven.foreignTojos();
        MatcherAssert.assertThat(
            String.format(DiscoverMojoTest.SHOULD_CONTAIN, DiscoverMojoTest.TEXT),
            tojos.contains(DiscoverMojoTest.TEXT),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(DiscoverMojoTest.SHOULD_NOT, stdout),
            tojos.contains(stdout),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            String.format(DiscoverMojoTest.SHOULD_CONTAIN, nop),
            tojos.contains(nop),
            Matchers.is(true)
        );
    }

    @Test
    void discoversWithSeveralObjectsWithDifferentVersions(
        @TempDir final Path tmp
    ) throws IOException {
        final Map<String, CommitHash> hashes = new CommitHashesMap.Fake();
        final FakeMaven maven = new FakeMaven(tmp)
            .with("withVersions", true)
            .withProgram(
                "+alias org.eolang.txt.sprintf\n",
                "# This is the default 64+ symbols comment in front of named abstract object.",
                "[] > main",
                "  seq > @",
                "    QQ.io.stdout",
                "      sprintf|0.28.1",
                "        \"Hello from %s\"",
                "        \"0.28.1\"",
                "    QQ.io.stdout",
                "      sprintf|0.28.2",
                "        \"Hello from %s\"",
                "        \"0.28.2\"",
                "    nop"
            )
            .execute(new FakeMaven.Discover());
        final ObjectName first = new OnVersioned("org.eolang.txt.sprintf", hashes.get("0.28.1"));
        final ObjectName second = new OnVersioned("org.eolang.txt.sprintf", hashes.get("0.28.2"));
        final ForeignTojos tojos = maven.foreignTojos();
        MatcherAssert.assertThat(
            String.format(DiscoverMojoTest.SHOULD_CONTAIN, first),
            tojos.contains(first),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(DiscoverMojoTest.SHOULD_CONTAIN, second),
            tojos.contains(second),
            Matchers.is(true)
        );
    }

    @Test
    void discoversDifferentUnversionedObjectsFromDifferentVersionedObjects(@TempDir final Path tmp)
        throws IOException {
        final Map<String, CommitHash> hashes = new CommitHashesMap.Fake();
        final String first = String.join(
            "\n",
            "# This is the default 64+ symbols comment in front of named abstract object.",
            "[] > sprintf",
            "  text > @"
        );
        final String second = String.join(
            "\n",
            "# This is the default 64+ symbols comment in front of named abstract object.",
            "[] > sprintf",
            "  text|0.28.5 > @"
        );
        final String one = hashes.get("0.28.1").value();
        final String two = hashes.get("0.28.2").value();
        final String three = hashes.get("0.28.5").value();
        final String string = "org.eolang.text";
        final String object = "foo.x.sprintf";
        final ForeignTojos tojos = new FakeMaven(tmp)
            .with("withVersions", true)
            .withProgram(first, new OnVersioned(object, one))
            .withProgram(second, new OnVersioned(object, two))
            .withProgram(first, new OnDefault(object))
            .execute(new FakeMaven.Discover())
            .foreignTojos();
        MatcherAssert.assertThat(
            String.format(
                "Tojos should contained 3 similar objects %s: 2 with different hashes %s and one without; but they didn't",
                string,
                Arrays.toString(new String[]{one, three})
            ),
            tojos.contains(
                new OnVersioned(string, one),
                new OnVersioned(string, three),
                new OnDefault(string)
            ),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotDiscoverWithVersions(@TempDir final Path tmp) throws IOException {
        final FakeMaven maven = new FakeMaven(tmp)
            .with("withVersions", false)
            .withVersionedProgram()
            .execute(new FakeMaven.Discover());
        final ObjectName seq = new OnVersioned("org.eolang.seq", "6c6269d");
        MatcherAssert.assertThat(
            String.format(DiscoverMojoTest.SHOULD_NOT, seq),
            maven.foreignTojos().contains(seq),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            String.format(DiscoverMojoTest.SHOULD_NOT, DiscoverMojoTest.TEXT),
            maven.foreignTojos().contains(DiscoverMojoTest.TEXT),
            Matchers.is(false)
        );
    }
}
