/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.io.ResourceOf;
import org.cactoos.scalar.LengthOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link DpsDepgraph.DpsJson}.
 *
 * @since 0.28.11
 */
@ExtendWith(MktmpResolver.class)
final class DpsDepgraphTest {

    @ParameterizedTest
    @CsvSource({
        "eo-math-dependencies-transient-dependency.json, 3",
        "eo-math-dependencies-without-foreign.json, 7"
    })
    void readsAllDependenciesFromJsonFile(
        final String name,
        final long number,
        @Mktmp final Path tmp
    ) throws Exception {
        MatcherAssert.assertThat(
            "The number of dependencies must be correct, but it isn't",
            new LengthOf(new DpsDepgraph.DpsJson(this.file(tmp, name))).value(),
            Matchers.equalTo(number)
        );
    }

    private Path file(final Path tmp, final String name) {
        try {
            return new Saved(
                new ResourceOf(
                    String.format("org/eolang/maven/dependencies/%s", name)
                ),
                tmp.resolve(name)
            ).value();
        } catch (final IOException ex) {
            throw new IllegalStateException(ex);
        }
    }
}
