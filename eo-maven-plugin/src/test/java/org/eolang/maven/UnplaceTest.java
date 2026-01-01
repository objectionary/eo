/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Unplace}.
 *
 * @since 0.11
 */
final class UnplaceTest {

    @ParameterizedTest
    @CsvSource({
        "/tmp/foo/bar, /tmp/foo/bar/a/b/c.eo, a.b.c",
        "/tmp/foo/bar, /tmp/foo/bar/a/b/.cd.ef.eo, a.b..cd.ef"
    })
    void makesName(
        final String base,
        final String source,
        final String name
    ) {
        MatcherAssert.assertThat(
            "Unplace must make the correct name, but it doesn't",
            new Unplace(Paths.get(base)).make(
                Paths.get(source)
            ),
            Matchers.equalTo(name)
        );
    }

}
