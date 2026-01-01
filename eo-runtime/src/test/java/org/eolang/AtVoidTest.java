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
 * Test case for {@link AtVoid}.
 * @since 0.16
 */
final class AtVoidTest {
    @Test
    void copiesWithoutStateAndException() {
        Assertions.assertDoesNotThrow(
            () -> new AtVoid("void").copy(new PhDefault()),
            "AtVoid without state must be copied successfully without exception"
        );
    }

    @Test
    void copiesInnerState() {
        final Phi state = new PhDefault();
        final Attr attr = new AtVoid("first");
        attr.put(state);
        MatcherAssert.assertThat(
            "AtVoid must copy its inner state on copying self",
            attr.copy(new PhDefault()).get(),
            Matchers.not(Matchers.equalTo(state))
        );
    }

    @Test
    void throwsOnGettingUnsetAttribute() {
        Assertions.assertThrows(
            ExUnset.class,
            () -> new AtVoid("attr").get(),
            "AtVoid must throw an exception when getting unset attribute"
        );
    }

    @Test
    void putsAndReturnsObject() {
        final Phi obj = new PhDefault();
        final Attr attr = new AtVoid("foo");
        attr.put(obj);
        MatcherAssert.assertThat(
            "AtVoid must return previously set object",
            attr.get(),
            Matchers.equalTo(obj)
        );
    }

    @Test
    void throwsOnReset() {
        final Attr attr = new AtVoid("bar");
        attr.put(new PhDefault());
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> attr.put(new PhDefault()),
            "AtVoid must throw an exception when resetting attribute"
        );
    }
}
