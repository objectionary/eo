/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * An object whose φ keeps making a copy of itself, until the counter runs out.
 *
 * @since 0.1.0
 */
final class EndlessRecursion extends PhDefault {

    /**
     * How many more times to recurse.
     */
    static final AtomicInteger COUNT = new AtomicInteger();

    /**
     * Ctor.
     */
    EndlessRecursion() {
        super(
            new Attrs(
                new Attr(
                    Phi.PHI,
                    new AtComposite(
                        new PhDefault(),
                        self -> {
                            final Phi result;
                            if (EndlessRecursion.COUNT.decrementAndGet() <= 0) {
                                result = new Data.ToPhi(0L);
                            } else {
                                result = new EndlessRecursion().copy();
                            }
                            return result;
                        }
                    )
                )
            )
        );
    }
}
