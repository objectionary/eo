/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Ref}.
 *
 * @since 0.75.0
 */
final class RefTest {

    @Test
    void writesOnlyTheLocatorOfWhatItCopies() {
        MatcherAssert.assertThat(
            "a copy must not carry anything besides its locator, but it does",
            new Xembler(
                new Directives().add("type").append(new Ref("Φ.vb7r.k2-qe.φ.ρ.ρ").directives())
            ).xmlQuietly(),
            XhtmlMatchers.hasXPath("/type/ref[@loc='Φ.vb7r.k2-qe.φ.ρ.ρ' and not(node())]")
        );
    }
}
