/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * An object with a void, a φ of its own and a child object under `kid`.
 *
 * @since 0.1.0
 */
final class Foo extends PhDefault {

    /**
     * Ctor.
     */
    Foo() {
        super(
            new Attrs(
                new Attr("x", new AtVoid("x")),
                new Attr(
                    "kid",
                    new AtComposite(new PhDefault(), rho -> new Kid())
                ),
                new Attr(
                    "φ",
                    new AtComposite(new PhDefault(), rho -> new Data.ToPhi(5L))
                )
            )
        );
    }
}
