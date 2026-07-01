/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhTerminator}.
 * @since 0.73.1
 */
final class PhTerminatorTest {

    @Test
    void failsWhenDataized() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(new PhTerminator()).take(),
            "dataizing the bottom object must abort instead of returning data"
        );
    }

    @Test
    void propagatesOnDispatch() {
        MatcherAssert.assertThat(
            "dispatching an attribute on the bottom object must propagate another bottom, not abort",
            new PhTerminator().take("any"),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void copiesIntoAnotherTerminator() {
        MatcherAssert.assertThat(
            "copying the bottom object must yield another bottom, not abort",
            new PhTerminator().copy(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void toleratesBinding() {
        Assertions.assertDoesNotThrow(
            () -> new PhTerminator().put(0, new PhTerminator()),
            "binding an attribute into the bottom object must be a no-op, not abort"
        );
    }
}
