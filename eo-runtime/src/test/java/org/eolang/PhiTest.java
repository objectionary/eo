/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Phi}.
 * @since 0.22
 */
final class PhiTest {

    @Test
    void takesPackage() {
        MatcherAssert.assertThat(
            "Phi should resolve and invoke method from root stdout object, but it didn't",
            new Dataized(
                new PhDispatch(
                    new PhApplication(
                        Phi.Φ.take("stdout"),
                        0,
                        new Data.ToPhi("Hello, world")
                    ),
                    "text"
                )
            ).asString(),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    void takesStandardPackage() {
        MatcherAssert.assertThat(
            "Phi should resolve and invoke method from org.eolang.stdout object, but it didn't",
            new Dataized(
                new PhDispatch(
                    new PhApplication(
                        Phi.Φ.take("stdout").copy(),
                        0, new Data.ToPhi("Hello, world")
                    ),
                    "text"
                )
            ).asString(),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    void takesDirectly() {
        MatcherAssert.assertThat(
            "Phi should resolve nested attribute nan.is-finite and return false, but it didn't",
            new Dataized(
                Phi.Φ.take("nan").take("is-finite")
            ).asBool(),
            Matchers.equalTo(false)
        );
    }

    @Test
    void getsLocation() {
        MatcherAssert.assertThat(
            "Phi should return correct locator, but it didn't",
            new PhSafe(
                Phi.Φ,
                "foobar",
                123,
                56,
                "Φ.org.eolang$obj",
                "obj"
            ).locator(),
            Matchers.equalTo("Φ.org.eolang$obj:123:56")
        );
    }

    @Test
    void getsForma() {
        MatcherAssert.assertThat(
            "Phi should delegate forma to the wrapped object, but it didn't",
            new PhSafe(
                new Data.ToPhi(42L),
                "foobar",
                123,
                56,
                "Φ.org.eolang$obj",
                "obj.x"
            ).forma(),
            Matchers.equalTo("Φ.number")
        );
    }
}
