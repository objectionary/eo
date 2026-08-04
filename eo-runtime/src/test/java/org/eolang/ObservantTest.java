/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Observant}, checking that {@link PhDefault#add(String, Attribute)}
 * is overridable and a subclass observes every attribute addition (#6273).
 * @since 0.1
 */
final class ObservantTest {

    @Test
    void letsSubclassObserveConstructorSuppliedAttributeOnFirstAccess() {
        final Observant kid = new Observant();
        kid.take("x");
        MatcherAssert.assertThat(
            "Overridden add() must see the constructor-supplied attribute once it is lazily materialized, but it didnt",
            kid.seen(),
            Matchers.hasItems("x")
        );
    }

    @Test
    void letsSubclassObserveExplicitAttributeAddition() {
        final Observant kid = new Observant();
        kid.add("extra", new AtVoid("extra"));
        MatcherAssert.assertThat(
            "Overridden add() must see a directly added attribute, but it didnt",
            kid.seen(),
            Matchers.hasItems("extra")
        );
    }
}
