/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.security.SecureRandom;

/**
 * An object whose φ is a fresh random number.
 *
 * <p>Every dataization of a copy asks {@link SecureRandom} again, so the
 * object tells apart a value that was computed once from one that was
 * computed twice.</p>
 *
 * @since 0.1.0
 */
final class PhRnd extends PhDefault {

    /**
     * Ctor.
     */
    PhRnd() {
        super(
            new Attrs(
                new Attr(
                    "φ",
                    new AtComposite(
                        new PhDefault(),
                        self -> new Data.ToPhi(new SecureRandom().nextDouble())
                    )
                )
            )
        );
    }
}
