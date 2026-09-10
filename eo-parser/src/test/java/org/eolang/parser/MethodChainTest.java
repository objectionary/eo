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
    void writesNamedPlainMethodLink() {
        final Emit emit = new Emit();
        emit.object("wrap", null, 1, 0);
        new MethodChain("bar", 3, false).write(emit, 2, "result");
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "plain method link was not emitted from its stored shape",
            MethodChainTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='wrap']/o[@base='.bar' and @name='result' and @line='2' and @pos='3' and @method='' and not(@fragile)]"
            )
        );
    }

    @Test
    void writesUnnamedFragileMethodLink() {
        final Emit emit = new Emit();
        emit.object("wrap", null, 1, 0);
        new MethodChain("read", 5, true).write(emit, 3, null);
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "fragile method link lost its marker or became named",
            MethodChainTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='wrap']/o[@base='.read' and @line='3' and @pos='5' and @method='' and @fragile='' and not(@name)]"
            )
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
