/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * An object with a void attribute, a kid, and a number as φ.
 *
 * <p>Its void attribute stands at the first position, so a second put into
 * it is the one the read-only rule must refuse.</p>
 *
 * @since 0.1.0
 */
final class PhParent extends PhDefault {

    /**
     * Ctor.
     */
    PhParent() {
        super(
            new Attrs(
                new Attr("x", new AtVoid("x")),
                new Attr(
                    "kid",
                    new AtComposite(new PhDefault(), rho -> new PhKid())
                ),
                new Attr(
                    "φ",
                    new AtComposite(new PhDefault(), rho -> new Data.ToPhi(5L))
                )
            )
        );
    }
}
