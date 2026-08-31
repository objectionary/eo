/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link LnBlank}.
 * @since 0.1
 */
final class LnBlankTest {

    @Test
    void incrementsPendingBlankCounter() {
        final Globals globals = new Globals();
        new LnBlank(new Span("", 1), false).into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "a blank line must bump pendingBlanks so the next line can read it",
            globals.pendingBlanks(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void incrementsTrailingBlankCounter() {
        final Globals globals = new Globals();
        new LnBlank(new Span("", 1), false).into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "a blank line must bump trailingBlanks so EOF can check the tail",
            globals.trailingBlanks(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void leavesStackUntouched() {
        final Stack stack = new Stack();
        new LnBlank(new Span("", 1), false).into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a blank line cannot push, pop, or otherwise change the stack",
            stack.empty(),
            Matchers.is(true)
        );
    }

    @Test
    void emitsNoDirectives() {
        final Emit emit = new Emit();
        new LnBlank(new Span("", 1), false).into(new Stack(), new Globals(), emit);
        MatcherAssert.assertThat(
            "a blank line cannot append directives — its only effect is the counter bump",
            emit.directives(),
            Matchers.emptyIterable()
        );
    }

    @Test
    void skipsTheConsecutiveBlanksRuleAtEndOfFile() {
        final Globals globals = new Globals();
        globals.blank();
        final Emit emit = new Emit();
        new LnBlank(new Span("", 2), true).into(new Stack(), globals, emit);
        MatcherAssert.assertThat(
            "a blank of the run that closes the file has no R-6.5.3 error of its own",
            emit.directives(),
            Matchers.emptyIterable()
        );
    }

    @Test
    void reportsTheSecondBlankInTheMiddleOfTheFile() {
        final Globals globals = new Globals();
        globals.blank();
        final Emit emit = new Emit();
        new LnBlank(new Span("", 2), false).into(new Stack(), globals, emit);
        MatcherAssert.assertThat(
            "two blanks between two non-blank lines are still an R-6.5.3 error",
            emit.directives(),
            Matchers.not(Matchers.emptyIterable())
        );
    }
}
