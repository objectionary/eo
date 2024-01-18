/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

package org.eolang;

/**
 * A attr-putting object.
 *
 * @since 0.1
 */
@Versionized
public final class PhWith extends PhOnce {

    /**
     * Ctor.
     *
     * @param phi The object
     * @param name The name of attr
     * @param attr The value
     */
    public PhWith(final Phi phi, final String name, final Phi attr) {
        super(
            () -> {
                phi.attr(name).put(attr);
                return phi;
            },
            () -> String.format(
                "%s[%s=%s]",
                phi,
                name,
                new Indented(attr)
            ),
            () -> {
                final String term = attr.φTerm();
                final String txt;
                if (term.contains("\n")) {
                    txt = String.format(
                        "%s(\n\t%s ↦ %s\n)",
                        phi.φTerm(),
                        name,
                        new Indented(term)
                    );
                } else {
                    txt = String.format(
                        "%s(%s ↦ %s)",
                        phi.φTerm(),
                        name,
                        term
                    );
                }
                return txt;
            }
        );
    }

    /**
     * Ctor.
     *
     * @param phi The object
     * @param pos The position
     * @param attr The value
     */
    public PhWith(final Phi phi, final int pos, final Phi attr) {
        super(
            () -> {
                phi.attr(pos).put(attr);
                return phi;
            },
            () -> String.format(
                "%s[#%d=%s]",
                phi,
                pos,
                new Indented(attr)
            ),
            () -> {
                final String term = attr.φTerm();
                final String txt;
                if (term.contains("\n")) {
                    txt = String.format(
                        "%s(\n\t#%d ↦ %s\n)",
                        phi.φTerm(),
                        pos,
                        new Indented(term)
                    );
                } else {
                    txt = String.format(
                        "%s(#%d ↦ %s)",
                        phi.φTerm(),
                        pos,
                        term
                    );
                }
                return txt;
            }
        );
    }

}
