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
 * Tests for the regex atom.
 * @since 0.57.4
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
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
        MatcherAssert.assertThat(
            "regex without closing slash should throw a clear ExFailure that mentions \"/\" and is not an opaque IndexOutOfBoundsException",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new PhCopy(Phi.Φ.take("tt.regex")),
                        "expression", new Data.ToPhi("/pattern")
                    ).take("compiled")
                ).take()
            ).toString(),
            Matchers.allOf(
                Matchers.containsString("/"),
                Matchers.not(Matchers.containsString("out of bounds"))
            )
        );
    }
}
