/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.security.SecureRandom;

/**
 * An object whose φ is a fresh random number on every dataization.
 *
 * @since 0.1.0
 */
final class Rnd extends PhDefault {

    /**
     * Ctor.
     */
    Rnd() {
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
