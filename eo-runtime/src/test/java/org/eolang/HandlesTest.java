/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.sun.jna.Pointer;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Handles}.
 * @since 0.76.0
 */
final class HandlesTest {

    @Test
    void givesBackThePointerItKeeps() {
        final Pointer pointer = new Pointer(42L);
        MatcherAssert.assertThat(
            "the number handed out must lead back to the very pointer it was given for",
            Handles.INSTANCE.get(
                "the handle", Handles.INSTANCE.add(pointer)
            ),
            Matchers.equalTo(pointer)
        );
    }

    @Test
    void namesEveryPointerApart() {
        MatcherAssert.assertThat(
            "two pointers kept at once must not answer to the same number",
            Handles.INSTANCE.add(new Pointer(1L)),
            Matchers.not(Matchers.equalTo(Handles.INSTANCE.add(new Pointer(2L))))
        );
    }

    @Test
    void forgetsAPointerOnceItIsRemoved() {
        final int handle = Handles.INSTANCE.add(new Pointer(7L));
        Handles.INSTANCE.remove("the handle", handle);
        Assertions.assertThrows(
            ExFailure.class,
            () -> Handles.INSTANCE.get("the handle", handle),
            "a number whose pointer is gone must name nothing, so that it never reaches C again"
        );
    }

    @Test
    void refusesANumberNamingNothing() {
        MatcherAssert.assertThat(
            "the failure must say which argument was wrong and what it was",
            Assertions.assertThrows(
                ExFailure.class,
                () -> Handles.INSTANCE.get("the 'dirp' argument", -1),
                "a number that never named a pointer was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("the 'dirp' argument"),
                Matchers.containsString("-1")
            )
        );
    }
}
