/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Iterator;
import org.cactoos.iterable.Mapped;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Error directives.
 * @since 0.50
 */
final class DrErrors implements Iterable<Directive> {

    /**
     * Errors accumulated.
     */
    private final Iterable<ParsingException> errors;

    /**
     * Ctor.
     * @param errors The errors.
     */
    DrErrors(final Iterable<ParsingException> errors) {
        this.errors = errors;
    }

    @Override
    public Iterator<Directive> iterator() {
        return new org.cactoos.iterable.Joined<>(
            new Mapped<Iterable<Directive>>(
                error -> new Directives()
                    .xpath("/object")
                    .strict(1)
                    .addIf("errors")
                    .strict(1)
                    .add("error")
                    .attr("check", "eo-parser")
                    .attr("line", error.line())
                    .attr("severity", "critical")
                    .set(error.getMessage())
                    .up()
                    .up(),
                this.errors
            )
        ).iterator();
    }
}
