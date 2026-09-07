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
 * @since 0.1
 */
final class LnVoidTest {

    @Test
    void emitsRhoForReceiverVoid() {
        final Emit emit = new Emit();
        new LnVoid(new Span("? > ^", 1)).into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `? > ^` receiver void must emit the same name `VoidName` promotes `^` to",
            LnVoidTest.render(emit),
            XhtmlMatchers.hasXPath(
                String.format(
                    "/object/o[@name='%s' and @base='∅']", new VoidName("^").asString()
                )
            )
        );
    }

    @Test
    void emitsPhiForAtVoidAttribute() {
        final Emit emit = new Emit();
        new LnVoid(new Span("? > @", 1)).into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `? > @` void attribute must emit the same name `VoidName` promotes `@` to",
            LnVoidTest.render(emit),
            XhtmlMatchers.hasXPath(
                String.format(
                    "/object/o[@name='%s' and @base='∅']", new VoidName("@").asString()
                )
            )
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
