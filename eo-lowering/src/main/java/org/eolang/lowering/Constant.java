/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;

/**
 * The value of one all-literal XMIR fragment, computed at build time.
 *
 * <p>The fragment is an application whose every leaf is a literal, such
 * as {@code 1.plus 1}: its value depends on nothing outside itself, so
 * computing it once here and splicing the result where the fragment
 * stood preserves the program exactly and spares the runtime the whole
 * object graph of the subexpression. The computing is one dataization of
 * the {@link Expression} holding the fragment, merged by phino with the
 * {@link Universe} that carries the tables; the forma of the value comes
 * with the bytes, the way {@link Datum} explains. Which methods can fold
 * is phino's knowledge, not ours: anything it refuses surfaces here as
 * an exception, which the caller treats as one fragment staying
 * unfolded, never as a broken build.</p>
 *
 * @since 0.76.0
 */
public final class Constant {

    /**
     * The binary that dataizes.
     */
    private final Phino phino;

    /**
     * The XMIR fragment to fold, an {@code <o/>} element.
     */
    private final Xnav fragment;

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param xmir The XMIR fragment to fold, an {@code <o/>} element
     */
    public Constant(final Phino exe, final Xnav xmir) {
        this.phino = exe;
        this.fragment = xmir;
    }

    /**
     * The value of the fragment.
     * @return The dataized bytes together with their forma
     * @throws IOException If the binary cannot be run
     */
    public Datum value() throws IOException {
        return this.phino.dataize(
            new Universe().text(),
            new Expression(this.fragment).text()
        );
    }
}
