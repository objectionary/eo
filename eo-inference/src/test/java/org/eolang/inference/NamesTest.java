/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Names}.
 *
 * @since 0.76.0
 */
final class NamesTest {

    @Test
    void takesFormationInScalarPackageAsScalar() {
        MatcherAssert.assertThat(
            "a method of a scalar package must be a scalar, but it wasnt",
            new Names(List.of(), List.of(), List.of("Φ.u16", "Φ.money")).scalar("Φ.money.plus"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotTakePackageThatOnlyStartsLikeScalar() {
        MatcherAssert.assertThat(
            "a package whose name only starts like a scalar one must not be a scalar, but it was",
            new Names(List.of(), List.of(), List.of("Φ.money")).scalar("Φ.moneybox.put"),
            Matchers.is(false)
        );
    }

    @Test
    void knowsRootOfListCode() {
        MatcherAssert.assertThat(
            "a package named a root must be one, but it wasnt",
            new Names(List.of(), List.of("Φ.queue", "Φ.bag"), List.of()).root("Φ.bag"),
            Matchers.is(true)
        );
    }
}
