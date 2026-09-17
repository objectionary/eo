/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * An object whose φ dataizes the object itself, until the counter runs out.
 *
 * @since 0.1.0
 */
final class RecursivePhi extends PhDefault {

    /**
     * How many more times to recurse.
     */
    static final AtomicInteger COUNT = new AtomicInteger();

    /**
     * Make one, with its φ in place.
     *
     * <p>The φ is attached here, and not in a constructor, because it is
     * an expression over the object itself, which does not exist yet
     * while its constructor runs.</p>
     *
     * @return The object
     */
    static Phi made() {
        final RecursivePhi made = new RecursivePhi();
        made.add(
            "φ",
            new AtComposite(
                made,
                rho -> {
                    final Phi result;
                    if (RecursivePhi.COUNT.decrementAndGet() <= 0) {
                        result = new Data.ToPhi(0L);
                    } else {
                        result = new Data.ToPhi(new Dataized(rho).asNumber());
                    }
                    return result;
                }
            )
        );
        return made;
    }
}
