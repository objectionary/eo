/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * An object whose second attribute is already cached.
 *
 * <p>Its first attribute is void and its second one is bound through
 * {@link AtOnce}, so a put at the second position finds nothing vacant
 * to fall through to and hits the cache instead.</p>
 *
 * @since 0.1.0
 */
final class PhCached extends PhDefault {

    /**
     * Ctor.
     */
    PhCached() {
        super(
            new Attrs(
                new Attr("x", new AtVoid("x")),
                new Attr("y", new AtOnce(new AtVoid("y")))
            )
        );
    }
}
