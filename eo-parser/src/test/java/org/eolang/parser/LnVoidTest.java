/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link LnVoid}.
 *
 * @since 0.1
 */
final class LnVoidTest {

    @Test
    void emitsRhoForReceiverVoid() {
        MatcherAssert.assertThat(
            "a `? > ^` receiver void must be emitted as ρ",
            LnVoidTest.parsed("  ? > ^ /Q.bytes"),
            XhtmlMatchers.hasXPath("/object/o[@name='atom']/o[@name='ρ' and @base='∅']")
        );
    }

    @Test
    void emitsPhiForAtVoid() {
        MatcherAssert.assertThat(
            "a `? > @` void inside an atom must be emitted as φ",
            LnVoidTest.parsed("  ? > @ /Q.bytes"),
            XhtmlMatchers.hasXPath("/object/o[@name='atom']/o[@name='φ' and @base='∅']")
        );
    }

    private static String parsed(final String line) {
        final Stack stack = new Stack();
        final Globals globals = new Globals();
        final Emit emit = new Emit();
        new LnFormation(new Span("[] > atom /Q.number", 1)).into(stack, globals, emit);
        new LnVoid(new Span(line, 2)).into(stack, globals, emit);
        emit.close();
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
