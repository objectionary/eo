/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.yegor256.Together;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CyclicBarrier;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link PhTerminator}.
 *
 * @since 0.73.1
 */
final class PhTerminatorTest {

    @Test
    void failsWhenDataized() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(new PhTerminator()).take(),
            "dataizing the terminator must abort instead of returning data"
        );
    }

    @Test
    void keepsOneCauseWhenManyThreadsPutAtOnce() {
        final int threads = 8;
        final Phi term = new PhTerminator();
        final Set<String> seen = Collections.synchronizedSet(new HashSet<>());
        final CyclicBarrier gate = new CyclicBarrier(threads);
        new Together<>(
            threads,
            thread -> {
                gate.await();
                term.put(0, new Data.ToPhi(String.format("cause %d", thread)));
                seen.add(
                    Assertions.assertThrows(
                        ExFailure.class,
                        () -> new Dataized(term).take(),
                        "forcing the terminator must abort"
                    ).getMessage()
                );
                return true;
            }
        ).asList();
        MatcherAssert.assertThat(
            "the cause is written once, so every thread must be told the same reason, but they werent",
            seen,
            Matchers.hasSize(1)
        );
    }

    @Test
    void propagatesOnDispatch() {
        MatcherAssert.assertThat(
            "dispatching an attribute on the terminator must propagate another one, not abort",
            new PhTerminator().take("any"),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void copiesIntoAnotherTerminator() {
        MatcherAssert.assertThat(
            "copying the terminator must yield another one, not abort",
            new PhTerminator().copy(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void toleratesBinding() {
        Assertions.assertDoesNotThrow(
            () -> new PhTerminator().put(0, new PhTerminator()),
            "putting an object into the terminator must not abort"
        );
    }

    @Test
    void reportsTheGivenCauseOnPanic() {
        final PhTerminator terminator = new PhTerminator();
        terminator.put(0, new Data.ToPhi("cannot proceed here"));
        MatcherAssert.assertThat(
            "forcing the terminator must not hide the cause that was put into it",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(terminator).take()
            ).getMessage(),
            Matchers.containsString("cannot proceed here")
        );
    }

    @Test
    void preservesPercentSignsInCause() {
        final String cause = "100% complete";
        final PhTerminator terminator = new PhTerminator(new Data.ToPhi(cause));
        MatcherAssert.assertThat(
            "forcing the terminator must treat its cause as literal text",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(terminator).take()
            ).getMessage(),
            Matchers.equalTo(cause)
        );
    }

    @Test
    void keepsTheFirstCause() {
        final PhTerminator terminator = new PhTerminator();
        terminator.put(0, new Data.ToPhi("the birth reason"));
        terminator.put(0, new Data.ToPhi("a later reason"));
        MatcherAssert.assertThat(
            "a later object put into the terminator must not overwrite its first cause",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(terminator).take()
            ).getMessage(),
            Matchers.containsString("the birth reason")
        );
    }

    @Test
    void hidesTheCauseFromTake() {
        final PhTerminator terminator = new PhTerminator();
        terminator.put(0, new Data.ToPhi("secret cause"));
        MatcherAssert.assertThat(
            "taking an attribute must not hand back the cause, only another terminator",
            terminator.take("cause"),
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
                        new Data.ToPhi("%d").take("printf").copy(),
                        "args", new Data.ToPhi(new Phi[]{new Data.ToPhi(number)})
                    )
                ).take()
            ).toString(),
            Matchers.containsString("doesn't fit into the long range")
        );
    }

    @Test
    void namesTheNumberOutOfTheLongRange() {
        MatcherAssert.assertThat(
            "the number out of the long range is not named in the reason",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new Data.ToPhi("%d").take("printf").copy(),
                        "args", new Data.ToPhi(new Phi[]{new Data.ToPhi(1.0e19)})
                    )
                ).take()
            ).toString(),
            Matchers.containsString("1.000000e19")
        );
    }

    @Test
    void keepsDispatchArgumentsOutOfTheCause() {
        MatcherAssert.assertThat(
            "a terminator reached by a dispatch names the argument it got, not the termination",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhDispatch(new PhTerminator(), "if"),
                        new Bind(0, new Data.ToPhi("первый")),
                        new Bind(1, new Data.ToPhi("второй"))
                    )
                ).take()
            ).getMessage(),
            Matchers.containsString("terminated computation")
        );
    }

    @Test
    void carriesTheCauseThroughDispatch() {
        MatcherAssert.assertThat(
            "the reason a terminator was born with is lost once it travels through a dispatch",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhDispatch(new PhTerminator(new Data.ToPhi("cannot open Ω")), "eq"),
                        new Bind(0, new Data.ToPhi("другое"))
                    )
                ).take()
            ).getMessage(),
            Matchers.equalTo("cannot open Ω")
        );
    }

    @Test
    void toleratesPutAtOtherPositions() {
        Assertions.assertDoesNotThrow(
            () -> new PhTerminator().put(1, new Data.ToPhi("nope")),
            "putting into the terminator away from position 0 must not abort"
        );
    }

    @Test
    void rejectsPutByName() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhTerminator().put("cause", new Data.ToPhi("nope")),
            "putting into the terminator by name, even cause, must abort"
        );
    }

    @Test
    void toleratesRhoBinding() {
        Assertions.assertDoesNotThrow(
            () -> new PhTerminator().put(Phi.RHO, new PhDefault()),
            "binding ρ onto the terminator must not abort"
        );
    }

    @Test
    void resolvesADataValueToNonTerminator() {
        MatcherAssert.assertThat(
            "resolving a real data value must not be seen as a terminator",
            new Data.ToPhi(42L).normalized(),
            Matchers.not(Matchers.instanceOf(PhTerminator.class))
        );
    }

    @Test
    void resolvesTheTerminatorToItself() {
        MatcherAssert.assertThat(
            "resolving the terminator must reveal a terminator",
            new PhTerminator().normalized(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void resolvesALazyDispatchToTerminator() {
        MatcherAssert.assertThat(
            "a lazy dispatch that yields a terminator must resolve to a PhTerminator lazily",
            new PhDispatch(new PhDefault(), "missing").normalized(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }
}
