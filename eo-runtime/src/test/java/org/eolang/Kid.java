/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The kid of {@link Foo}, for the tests.
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
