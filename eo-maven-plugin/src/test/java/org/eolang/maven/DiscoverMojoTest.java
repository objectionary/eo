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

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.tojos.MnCsv;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.tojos.ForeignTojo;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link DiscoverMojo}.
 *
 * @since 0.28.11
 */
@ExtendWith(MktmpResolver.class)
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
        @Mktmp final Path tmp
    ) throws IOException {
        final FakeMaven maven = new FakeMaven(tmp);
        maven.withProgram(new ResourceOf(program))
            .execute(new FakeMaven.Discover());
        final Deque<Map<String, String>> json = new LinkedList<>(
            new MnCsv(maven.foreignPath()).read()
        );
        final Map<String, String> first = json.removeFirst();
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            dependencies,
            Matchers.equalTo(json.size())
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            String.valueOf(dependencies),
            Matchers.equalTo(first.get("discovered"))
        );
    }

    @Test
    void discoversForDifferentScopes(@Mktmp final Path tmp) throws IOException {
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
}
