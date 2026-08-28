/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link FakeMaven}.
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class FakeMavenTest {

    @Test
    void refusesAParameterNoMojoDeclares(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the name of the parameter must be in the complaint, so a rename is easy to find",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new FakeMaven(temp).with("thereIsNoSuchParameter", true),
                "a parameter no mojo declares would be dropped and the test would pass on"
            ).getMessage(),
            Matchers.containsString("thereIsNoSuchParameter")
        );
    }

    @Test
    void takesAParameterAMojoDeclares(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).with("sources", temp.toFile()),
            "a parameter a mojo does declare must be taken"
        );
    }
}
