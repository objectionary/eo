/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
                    .xpath("/program")
                    .strict(1)
                    .addIf("errors")
                    .strict(1)
                    .add("error")
                    .attr("check", "eo-parser")
                    .attr("line", error.line())
                    .attr("severity", "critical")
                    .set(error.getMessage()),
                this.errors
            )
        ).iterator();
    }
}
