/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * An object whose φ itself is void.
 *
 * <p>Nothing is bound to it until a put arrives, so the object shows what
 * a dataization does with a φ that is injected from the outside.</p>
 *
 * @since 0.1.0
 */
final class PhVoidPhi extends PhDefault {

    /**
     * Ctor.
     */
    PhVoidPhi() {
        super(new Attrs(new Attr(Phi.PHI, new AtVoid(Phi.PHI))));
    }
}
