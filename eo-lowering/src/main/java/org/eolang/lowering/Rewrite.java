/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;

/**
 * One lowering pass over one document.
 *
 * <p>It takes a document, rewrites in place whatever it knows how to turn
 * into a value or an atom, and answers how many fragments changed.
 * Whatever refuses is left as written. Passes run one after another over
 * the same document, so an earlier one shrinks what a later one sees.</p>
 *
 * @since 0.76.0
 */
@FunctionalInterface
interface Rewrite {

    /**
     * Rewrite the qualifying fragments of the document, in place.
     *
     * @param doc The XMIR document to rewrite
     * @return How many fragments were rewritten
     * @throws IOException If the pass cannot run
     */
    int rewrite(Xnav doc) throws IOException;
}
