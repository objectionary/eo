/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;

/**
 * One lowering pass over one XMIR document.
 *
 * <p>Every implementation walks the document, finds the fragments it
 * knows how to turn into values or synthetic atoms, and rewrites them in
 * place, leaving whatever refuses as written. The caller runs the passes
 * one after another over the same document, so an earlier pass shrinks
 * what a later one sees.</p>
 *
 * @since 0.76.0
 */
@FunctionalInterface
public interface Rewrite {

    /**
     * Rewrite the qualifying fragments of the document, in place.
     * @param doc The XMIR document to rewrite
     * @return How many fragments were rewritten
     * @throws IOException If the pass cannot run
     */
    int rewrite(Xnav doc) throws IOException;
}
