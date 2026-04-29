/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EOtt; // NOPMD

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhCopy;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link EOregex$EOφ}.
 *
 * @since 0.57.4
 * @checkstyle TypeNameCheck (3 lines)
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "PMD.AvoidDollarSigns"})
final class RegexAtomTest {

    @Test
    void compilesRegexWithSlashes() {
        MatcherAssert.assertThat(
            "regex \"/[a-z]+/\" should compile and match \"hello\"",
            new Dataized(
                new PhWith(
                    new PhCopy(
                        new PhWith(
                            new PhCopy(Phi.Φ.take("tt.regex")),
                            "expression", new Data.ToPhi("/[a-z]+/")
                        ).take("compiled").take("matches")
                    ),
                    "txt", new Data.ToPhi("hello")
                )
            ).asBool(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void throwsClearErrorOnMissingClosingSlash() {
        final ExAbstract error = Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                new PhWith(
                    new PhCopy(Phi.Φ.take("tt.regex")),
                    "expression", new Data.ToPhi("/pattern")
                ).take("compiled")
            ).take(),
            "regex without closing slash should throw a clear ExFailure, but did not"
        );
        MatcherAssert.assertThat(
            "Error should mention the missing closing slash, but it did not",
            error.toString(),
            Matchers.containsString("/")
        );
        MatcherAssert.assertThat(
            "Error must not be an opaque IndexOutOfBoundsException",
            error.toString(),
            Matchers.not(Matchers.containsString("out of bounds"))
        );
    }
}
