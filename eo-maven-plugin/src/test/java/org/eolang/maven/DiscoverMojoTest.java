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
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.hash.ChsAsMap;
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

    @ParameterizedTest
    @CsvSource({
        "org/eolang/maven/mess.eo, 7",
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
            .with("commitHashes", new ChsAsMap.Fake())
            .withProgram(
                "+alias org.eolang.txt.sprintf\n",
                "[] > main",
                "  seq > @",
                "    QQ.io.stdout|0.28.9",
                "      sprintf|0.28.5",
                "        \"Hello world\"",
                "          TRUE",
                "    nop"
            )
            .execute(new FakeMaven.Discover());
        final String sprintf = "org.eolang.txt.sprintf|9c93528";
        final String stdout = "org.eolang.io.stdout|be83d9a";
        final String nop = "org.eolang.nop";
        final ForeignTojos tojos = maven.externalTojos();
        MatcherAssert.assertThat(
            String.format(
                "External tojos should have contained %s object after discovering, but they didn't",
                sprintf
            ),
            tojos.contains(sprintf),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "External tojos should have not contained %s object after discovering, but they did",
                stdout
            ),
            tojos.contains(stdout),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            String.format(
                "External tojos should have contained %s object after discovering, but they didn't",
                nop
            ),
            tojos.contains(nop),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotDiscoverWithVersions(@TempDir final Path tmp) throws IOException {
        final FakeMaven maven = new FakeMaven(tmp)
            .with("withVersions", false)
            .with("failOnError", false)
            .with("commitHashes", new ChsAsMap.Fake())
            .withProgram(
                "+alias org.eolang.txt.sprintf\n",
                "[] > main",
                "  seq > @",
                "    QQ.io.stdout|0.28.9",
                "      sprintf|0.28.5",
                "        \"Hello world\"",
                "          TRUE"
            )
            .execute(new FakeMaven.Discover());
        final String sprintf = "org.eolang.txt.sprintf|9c93528";
        final String stdout = "org.eolang.io.stdout|be83d9a";
        MatcherAssert.assertThat(
            String.format(
                "External tojos should not have contained %s object after discovering, but they did",
                sprintf
            ),
            maven.externalTojos().contains(sprintf),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            String.format(
                "External tojos should not have contained %s object after discovering, but they did",
                stdout
            ),
            maven.externalTojos().contains(stdout),
            Matchers.is(false)
        );
    }
}
