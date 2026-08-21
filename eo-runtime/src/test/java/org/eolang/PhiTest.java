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
    void tellsWhyANonFiniteNumberHasNoDecimalForm() {
        MatcherAssert.assertThat(
            "asking nan for its decimal form must not lose the reason written for that moment",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new Data.ToPhi(Double.NaN).take("as-decimal")
                ).take()
            ).getMessage(),
            Matchers.containsString("Can't write a non-finite number as decimal")
        );
    }

    @Test
    void tellsWhyAFractionalPartitionCountIsRefused() {
        MatcherAssert.assertThat(
            "integrating over 1.5 partitions must not lose the reason written for that moment",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        Phi.Φ.take("number.integral").copy(),
                        "n",
                        new Data.ToPhi(1.5)
                    )
                ).take()
            ).getMessage(),
            Matchers.containsString("the number of partitions must be a finite positive integer")
        );
    }

    @Test
    void tellsWhyAPrecisionBoundaryPartitionCountIsRefused() {
        MatcherAssert.assertThat(
            "integrating at the unit-step precision boundary must fail before the loop starts",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        Phi.Φ.take("number.integral").copy(),
                        "n",
                        new Data.ToPhi(9007199254740992D)
                    )
                ).take()
            ).getMessage(),
            Matchers.containsString("smaller than 2^53")
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
