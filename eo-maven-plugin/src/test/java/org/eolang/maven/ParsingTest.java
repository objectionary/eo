/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Parsing}.
 * @since 0.1
 */
final class ParsingTest {

    @Test
    void includesDigestInVersion() {
        MatcherAssert.assertThat(
            "the cache-key version must end with the digest of the known objects",
            ParsingTest.parsing().version("some-digest"),
            Matchers.endsWith("-some-digest")
        );
    }

    @Test
    void changesVersionWhenDigestChanges() {
        final Parsing parsing = ParsingTest.parsing();
        MatcherAssert.assertThat(
            "the cache-key version must reflect the digest of the known objects it is given",
            parsing.version("digest-one"),
            Matchers.not(Matchers.equalTo(parsing.version("digest-two")))
        );
    }

    /**
     * A minimal {@link Parsing} instance for testing its private helpers.
     * @return A new instance
     */
    private static Parsing parsing() {
        return new Parsing(
            new TjsForeign(),
            Paths.get("target"),
            Paths.get("target/cache"),
            true,
            "1.0-SNAPSHOT",
            Paths.get("src/main/eo")
        );
    }
}
