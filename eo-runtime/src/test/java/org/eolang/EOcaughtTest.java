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
 * Test case for {@link EOcaught}.
 *
 * @since 0.0.0
 */
final class EOcaughtTest {

    @Test
    void handsTheMessageOfATerminatorToTheAlternative() {
        final EOcaught caught = new EOcaught();
        caught.put("value", new PhTerminator(new Data.ToPhi("the reason it stopped")));
        caught.put("alternative", new PhTerminator());
        MatcherAssert.assertThat(
            "the alternative must be handed the message the termination carried, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(caught.lambda()).asString(),
                "an alternative that terminates must terminate the whole computation"
            ).getMessage(),
            Matchers.equalTo("the reason it stopped")
        );
    }

    @Test
    void handsTheMessageOfAForcedTerminationToTheAlternative() {
        final EOcaught caught = new EOcaught();
        caught.put(
            "value",
            new PhDefault() {
                @Override
                public Phi normalized() {
                    throw new ExFailure("the step that stopped");
                }
            }
        );
        caught.put("alternative", new PhTerminator());
        MatcherAssert.assertThat(
            "the alternative must be handed the message of a termination that was forced, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(caught.lambda()).asString(),
                "an alternative that terminates must terminate the whole computation"
            ).getMessage(),
            Matchers.equalTo("the step that stopped")
        );
    }

    @Test
    void keepsTheValueWhenNothingTerminates() {
        final EOcaught caught = new EOcaught();
        caught.put("value", new Data.ToPhi(42L));
        caught.put("alternative", new PhTerminator());
        MatcherAssert.assertThat(
            "the value must come through untouched when nothing terminates, but it didnt",
            new Dataized(caught.lambda()).asNumber(),
            Matchers.equalTo(42.0)
        );
    }

    @Test
    void refusesAnAlternativeThatCannotTakeTheMessage() {
        final EOcaught caught = new EOcaught();
        caught.put("value", new PhTerminator(new Data.ToPhi("the reason it stopped")));
        caught.put("alternative", new Data.ToPhi(42L));
        MatcherAssert.assertThat(
            "an alternative with no void to take the message must abort, but it didnt",
            Assertions.assertThrows(
                ExAbstract.class,
                caught::lambda,
                "an alternative that cannot take the message must not swallow it"
            ).getMessage(),
            Matchers.containsString("void attribute")
        );
    }
}
