/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

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
            "putting an object into the bottom object must not abort"
        );
    }

    @Test
    void reportsTheGivenCauseOnPanic() {
        final PhTerminator bottom = new PhTerminator();
        bottom.put(0, new Data.ToPhi("cannot proceed here"));
        MatcherAssert.assertThat(
            "forcing the bottom object must not hide the cause that was put into it",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(bottom).take()
            ).getMessage(),
            Matchers.containsString("cannot proceed here")
        );
    }

    @Test
    void preservesPercentSignsInCause() {
        final String cause = "100% complete";
        final PhTerminator bottom = PhTerminator.withCause(cause);
        MatcherAssert.assertThat(
            "forcing the bottom object must treat its cause as literal text",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(bottom).take()
            ).getMessage(),
            Matchers.equalTo(cause)
        );
    }

    @Test
    void keepsTheFirstCause() {
        final PhTerminator bottom = new PhTerminator();
        bottom.put(0, new Data.ToPhi("the birth reason"));
        bottom.put(0, new Data.ToPhi("a later reason"));
        MatcherAssert.assertThat(
            "a later object put into the bottom object must not overwrite its first cause",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(bottom).take()
            ).getMessage(),
            Matchers.containsString("the birth reason")
        );
    }

    @Test
    void hidesTheCauseFromTake() {
        final PhTerminator bottom = new PhTerminator();
        bottom.put(0, new Data.ToPhi("secret cause"));
        MatcherAssert.assertThat(
            "taking an attribute must not hand back the cause, only another bottom",
            bottom.take("cause"),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @ParameterizedTest
    @ValueSource(doubles = {9.3e18, 1.0e19, -1.0e19})
    void keepsTheReasonOfAnOutOfRangeDecimalConversion(final double number) {
        MatcherAssert.assertThat(
            String.format(
                "the number %s is not reported as out of the long range, but as something else",
                number
            ),
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        Phi.Φ.take("string.printf").copy(),
                        new Bind("format", new Data.ToPhi("%d")),
                        new Bind("args", new Data.ToPhi(new Phi[]{new Data.ToPhi(number)}))
                    )
                ).take()
            ).toString(),
            Matchers.containsString("doesn't fit into the long range")
        );
    }

    @Test
    void toleratesPutAtOtherPositions() {
        Assertions.assertDoesNotThrow(
            () -> new PhTerminator().put(1, new Data.ToPhi("nope")),
            "putting into the bottom object away from position 0 must not abort"
        );
    }

    @Test
    void rejectsPutByName() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhTerminator().put("cause", new Data.ToPhi("nope")),
            "putting into the bottom object by name, even cause, must abort"
        );
    }

    @Test
    void toleratesRhoBinding() {
        Assertions.assertDoesNotThrow(
            () -> new PhTerminator().put(Phi.RHO, new PhDefault()),
            "binding ρ onto the bottom object must not abort"
        );
    }

    @Test
    void resolvesADataValueToNonBottom() {
        MatcherAssert.assertThat(
            "resolving a real data value must not be seen as a bottom",
            new Data.ToPhi(42L).normalized(),
            Matchers.not(Matchers.instanceOf(PhTerminator.class))
        );
    }

    @Test
    void resolvesTheBottomToItself() {
        MatcherAssert.assertThat(
            "resolving the bottom object must reveal a bottom",
            new PhTerminator().normalized(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void resolvesALazyDispatchToBottom() {
        MatcherAssert.assertThat(
            "a lazy dispatch that yields a bottom must resolve to a PhTerminator without forcing",
            new PhDispatch(new PhDefault(), "missing").normalized(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }
}
