/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.function.Function;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link Phi}.
 * @since 0.22
 */
final class PhiTest {

    @ParameterizedTest
    @MethodSource("decorators")
    void keepsDecoratorDistinctFromItsOrigin(
        final String name, final Function<Phi, Phi> decorator
    ) {
        final Phi origin = new PhDefault();
        final Phi wrapped = decorator.apply(origin);
        MatcherAssert.assertThat(
            String.format("the %s decorator must not equal its origin", name),
            wrapped.equals(origin),
            Matchers.equalTo(false)
        );
    }

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
    void takesLambdaOfNonAtomAsAbsent() {
        MatcherAssert.assertThat(
            "Taking λ from a non-atom must return a terminator, not throw a cast error",
            new PhDefault().take(Phi.LAMBDA),
            Matchers.instanceOf(PhTerminator.class)
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
                        new Data.ToPhi(9_007_199_254_740_992d)
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

    private static Stream<Arguments> decorators() {
        return Stream.of(
            Arguments.of("again", (Function<Phi, Phi>) PhAgain::new),
            Arguments.of(
                "coverage", (Function<Phi, Phi>) phi -> new PhCoverage(phi, "Φ.x:1:1")
            ),
            Arguments.of("logged", (Function<Phi, Phi>) PhLogged::new),
            Arguments.of("loop", (Function<Phi, Phi>) PhLoop::new),
            Arguments.of("once", (Function<Phi, Phi>) phi -> new PhOnce(() -> phi)),
            Arguments.of("safe", (Function<Phi, Phi>) PhSafe::new),
            Arguments.of("sticky", (Function<Phi, Phi>) PhSticky::new)
        );
    }
}
