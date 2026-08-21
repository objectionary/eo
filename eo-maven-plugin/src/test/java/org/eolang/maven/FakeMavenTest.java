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
 * @since 0.1
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith(MktmpResolver.class)
final class FakeMavenTest {

    @Test
    void rejectsUnknownParameter(@Mktmp final Path temp) {
        final String unknown = "unknownParameter";
        MatcherAssert.assertThat(
            "The failure must identify the unknown parameter",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new FakeMaven(temp).with(unknown, true)
            ).getMessage(),
            Matchers.containsString(unknown)
        );
    }

    @Test
    void acceptsParameterDeclaredByAnyMojo(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).with("sources", temp.toFile()),
            "A field declared by MjPrint must be accepted"
        );
    }
}
