/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for the {@link Line} contract.
 *
 * <p>Verifies that any {@code Line} impl is invoked through the same
 * single-method shape and that the supplied {@code Stack}, {@code
 * Globals}, and {@code Emit} are all reachable from inside {@code
 * into}. *
 *
 * @since 0.1
 */
final class LineTest {

    @Test
    void deliversAllThreeArgumentsToImplementation() {
        final Line line = (stack, globals, emit) -> stack.push(
            0, 0, Kind.TOP_LEVEL, Openness.OPEN
        );
        final Stack stack = new Stack();
        line.into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "an impl must reach Stack through the supplied reference and push a level",
            stack.depth(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void allowsImplementationToMutateGlobals() {
        final Line line = (stack, globals, emit) -> globals.markEmitted();
        final Globals globals = new Globals();
        line.into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "an impl must reach Globals through the supplied reference and mutate it",
            globals.firstObjectEmitted(),
            Matchers.is(true)
        );
    }

    @Test
    void allowsImplementationToEmitDirectives() {
        final Line line = (stack, globals, emit) -> emit.error(1, 0, "boom");
        final Emit emit = new Emit();
        line.into(new Stack(), new Globals(), emit);
        MatcherAssert.assertThat(
            "an impl must reach Emit through the supplied reference and append directives",
            new Xembler(
                new Directives().add("object").append(emit.directives())
            ).xmlQuietly(),
            XhtmlMatchers.hasXPath("/object/errors/error")
        );
    }
}
