/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Digest}.
 * @since 0.76.0
 */
final class DigestTest {

    @Test
    void digestsBodyIntoTwelveHexCharacters() {
        MatcherAssert.assertThat(
            "the digest must be the first twelve hex of SHA-256, but it isnt",
            new Digest("return new Data.ToPhi(v0);").hex(),
            Matchers.equalTo("6fb6af12ec09")
        );
    }
}
