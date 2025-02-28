/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Phi}.
 *
 * @since 0.22
 */
final class PhiTest {

    @Test
    void takesPackage() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                new PhCopy(
                    new PhMethod(
                        new PhWith(
                            new PhCopy(
                                new PhMethod(
                                    new PhMethod(
                                        new PhMethod(
                                            Phi.Φ.take("org"),
                                            "eolang"
                                        ),
                                        "io"
                                    ),
                                    "stdout"
                                )
                            ),
                            0,
                            new Data.ToPhi("Hello, world")
                        ),
                        "text"
                    )
                )
            ).asString(),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    void takesStandardPackage() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                new PhCopy(
                    new PhMethod(
                        new PhWith(
                            new PhCopy(Phi.Φ.take("org.eolang.io.stdout")),
                            0, new Data.ToPhi("Hello, world")
                        ),
                        "text"
                    )
                )
            ).asString(),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    void takesDirectly() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                Phi.Φ.take("org").take("eolang").take("nan").take("gt")
            ).asBool(),
            Matchers.equalTo(false)
        );
    }

    @Test
    void getsLocation() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
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
            AtCompositeTest.TO_ADD_MESSAGE,
            new PhSafe(
                Phi.Φ,
                "foobar",
                123,
                56,
                "Φ.org.eolang$obj",
                "obj.x"
            ).forma(),
            Matchers.equalTo("org.eolang.obj.x")
        );
    }
}
