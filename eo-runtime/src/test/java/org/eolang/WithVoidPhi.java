/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * An object whose φ is a void, so it answers nothing until one is put there.
 *
 * @since 0.1.0
 */
final class WithVoidPhi extends PhDefault {

    /**
     * Ctor.
     */
    WithVoidPhi() {
        super(new Attrs(new Attr(Phi.PHI, new AtVoid(Phi.PHI))));
    }
}
