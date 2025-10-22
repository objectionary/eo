/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.stream.Collectors;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.cactoos.Text;

/**
 * Fully Qualified Name for compact arrays.
 * @since 0.57.2
 */
final class CompactArrayFqn implements Text {

    /**
     * Context.
     */
    private final EoParser.CompactArrayContext context;

    /**
     * Ctor.
     * @param input Input
     */
    CompactArrayFqn(final String input) {
        this(
            new EoParser(new CommonTokenStream(new EoLexer(CharStreams.fromString(input))))
                .compactArray()
        );
    }

    /**
     * Ctor.
     * @param ctx Context
     */
    CompactArrayFqn(final EoParser.CompactArrayContext ctx) {
        this.context = ctx;
    }

    @Override
    public String asString() {
        final String name = this.context.NAME().stream()
            .map(ParseTree::getText)
            .collect(Collectors.joining("."));
        final String fqn;
        if (this.context.HOME() == null && this.context.XI() == null) {
            fqn = name;
        } else if (this.context.HOME() != null && this.context.XI() == null) {
            fqn = String.format("Φ.org.eolang.%s", name);
        } else {
            fqn = String.format("ξ.%s", name);
        }
        return fqn;
    }
}
