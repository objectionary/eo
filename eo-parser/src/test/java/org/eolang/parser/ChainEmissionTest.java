/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link ChainEmission}.
 * @since 0.1
 */
final class ChainEmissionTest {

    @Test
    void emitsBareHeadWithoutMethodLinkWhenChainIsEmpty() {
        final Emit emit = new Emit();
        emit.object("foo", null, 1, 0);
        new ChainEmission(
            emit,
            new Span("bar > foo", 1),
            new Value(Value.Kind.IDENTIFIER, "bar", 6, 9),
            Collections.emptyList(),
            new Suffix("> foo", new Span("bar > foo", 1), 4)
        ).run();
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "an empty chain must emit just the head with the user-given name attached",
            ChainEmissionTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='foo']/o[@base='bar' and @name='foo']",
                "/object/o[@name='foo' and not(o/o)]"
            )
        );
    }

    @Test
    void emitsHeadAndSingleMethodLinkAsFlatSiblings() {
        final Emit emit = new Emit();
        emit.object("wrap", null, 1, 0);
        new ChainEmission(
            emit,
            new Span("foo.bar > wrap", 1),
            new Value(Value.Kind.IDENTIFIER, "foo", 0, 3),
            Collections.singletonList(new MethodChain("bar", 3, 7)),
            new Suffix("> wrap", new Span("foo.bar > wrap", 1), 8)
        ).run();
        emit.close();
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "a single-method chain must emit head and `.bar` as siblings, name on the link",
            ChainEmissionTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='wrap']/o[@base='foo' and not(@name)]",
                "/object/o[@name='wrap']/o[@base='.bar' and @method='' and @name='wrap']"
            )
        );
    }

    @Test
    void emitsConstantMarkerWhenSuffixCarriesBang() {
        final Emit emit = new Emit();
        emit.object("wrap", null, 1, 0);
        new ChainEmission(
            emit,
            new Span("bar > foo!", 1),
            new Value(Value.Kind.IDENTIFIER, "bar", 0, 3),
            Collections.emptyList(),
            new Suffix("> foo!", new Span("bar > foo!", 1), 4)
        ).run();
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "a `!` const suffix must add @const to the named element",
            ChainEmissionTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='wrap']/o[@base='bar' and @name='foo' and @const='']"
            )
        );
    }

    @Test
    void emitsIntermediateLinksWithoutNameAndLastLinkWithName() {
        final Emit emit = new Emit();
        emit.object("wrap", null, 1, 0);
        new ChainEmission(
            emit,
            new Span("foo.bar.baz > q", 1),
            new Value(Value.Kind.IDENTIFIER, "foo", 0, 3),
            Arrays.asList(
                new MethodChain("bar", 3, 7),
                new MethodChain("baz", 7, 11)
            ),
            new Suffix("> q", new Span("foo.bar.baz > q", 1), 12)
        ).run();
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "intermediate link must be unnamed; only the last `.baz` carries the user-given name",
            ChainEmissionTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='wrap']/o[@base='foo' and not(@name)]",
                "/object/o[@name='wrap']/o[@base='.bar' and @method='' and not(@name)]",
                "/object/o[@name='wrap']/o[@base='.baz' and @method='' and @name='q']"
            )
        );
    }

    /**
     * Render an {@link Emit} into XMIR for assertion.
     * @param emit Emitter to render
     * @return The rendered XMIR text
     */
    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
