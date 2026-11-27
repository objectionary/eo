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
 * Test case for {@link MethodChain}.
 *
 * @since 0.1
 */
final class MethodChainTest {

    @Test
    void emitsNamedOrdinaryLinkAtDotPosition() {
        final Emit emit = new Emit();
        new MethodChain("bar", 3, false).emit(emit, 7, "result");
        emit.close();
        MatcherAssert.assertThat(
            "an ordinary link must carry its dotted base, dot position, and name",
            MethodChainTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@base='.bar' and @line='7' and @pos='3' and @name='result' and @method='' and not(@fragile)]"
            )
        );
    }

    @Test
    void emitsUnnamedFragileLinkAtDotPosition() {
        final Emit emit = new Emit();
        new MethodChain("read", 9, true).emit(emit, 4, null);
        emit.close();
        MatcherAssert.assertThat(
            "a fragile link must have both markers and no name",
            MethodChainTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@base='.read' and @line='4' and @pos='9' and not(@name) and @method='' and @fragile='']"
            )
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
