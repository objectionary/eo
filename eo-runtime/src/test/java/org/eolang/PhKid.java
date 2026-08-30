/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * A child object with one void attribute and a boolean φ.
 *
 * <p>It is attached to {@link PhParent}, so that the parent has a kid whose
 * rho can be looked at.</p>
 *
 * @since 0.1.0
 */
final class PhKid extends PhDefault {

    /**
     * Ctor.
     */
    PhKid() {
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
