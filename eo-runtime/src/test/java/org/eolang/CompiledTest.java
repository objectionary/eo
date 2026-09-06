/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.PatternSyntaxException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Compiled}.
 * @since 0.77
 */
final class CompiledTest {

    @Test
    void handsBackThePatternItBuiltBefore() {
        final String source = String.format(
            "(?i)\\bζ%d[a-z]{2,}\\b", ThreadLocalRandom.current().nextInt()
        );
        MatcherAssert.assertThat(
            "a source built once must be handed back as the very pattern it was built into, or every match would compile it again",
            new Compiled(source).it(),
            Matchers.sameInstance(new Compiled(source).it())
        );
    }

    @Test
    void tellsOneSourceFromAnother() {
        final int seed = ThreadLocalRandom.current().nextInt();
        MatcherAssert.assertThat(
            "two sources must be built into two patterns, or a search would run the expression it was not asked for",
            new Compiled(String.format("ζ%d[0-9]{3}", seed)).it(),
            Matchers.not(
                Matchers.sameInstance(new Compiled(String.format("ζ%d[0-9]{4}", seed)).it())
            )
        );
    }

    @Test
    void refusesASourceTheEngineChokesOn() {
        Assertions.assertThrows(
            PatternSyntaxException.class,
            () -> new Compiled(
                String.format("ζ%d**", ThreadLocalRandom.current().nextInt())
            ).it(),
            "a source that does not compile must be refused, so that 'checked' can still name the construct the engine choked on"
        );
    }
}
