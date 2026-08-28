/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link MojoFields}.
 * @since 0.74.0
 */
final class MojoFieldsTest {

    @Test
    void findsTheNamesTheMojosDeclare() {
        MatcherAssert.assertThat(
            "a name every mojo of the plugin inherits must be found, but it wasnt",
            new MojoFields().all(),
            Matchers.hasItem("targetDir")
        );
    }

    @Test
    void findsNothingWhereNoMojoIsCompiled() {
        MatcherAssert.assertThat(
            "a prefix no class of the directory carries must find nothing at all",
            new MojoFields("ThereIsNoSuchPrefix").all(),
            Matchers.empty()
        );
    }
}
