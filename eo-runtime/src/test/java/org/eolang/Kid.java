/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The child object that {@link Foo} keeps under its `kid` attribute.
 *
 * @since 0.1.0
 */
final class Kid extends PhDefault {

    /**
     * Ctor.
     */
    Kid() {
        super(
            new Attrs(
                new Attr("z", new AtVoid("z")),
                new Attr(
                    Phi.PHI,
                    new AtComposite(new PhDefault(), rho -> new Data.ToPhi(true))
                )
            )
        );
    }
}
