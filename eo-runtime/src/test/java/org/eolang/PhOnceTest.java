/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhOnce}.
 * @since 0.60
 */
final class PhOnceTest {

    @Test
    void delegatesTermToWrappedObjectByDefault() {
        MatcherAssert.assertThat(
            "PhOnce without explicit term must delegate φ-term to the wrapped object, but it didnt",
            new PhOnce(() -> new PhDefault(new byte[] {(byte) 0x01})).φTerm(),
            Matchers.equalTo("[D> 01]")
        );
    }

    @Test
    void doesNotEvaluateWrappedObjectForTerm() {
        MatcherAssert.assertThat(
            "PhOnce with explicit term must render it without evaluating the wrapped object, but it didnt",
            new PhOnce(
                () -> {
                    throw new IllegalStateException("must not be evaluated");
                },
                () -> "x.foo"
            ).φTerm(),
            Matchers.equalTo("x.foo")
        );
    }
}
