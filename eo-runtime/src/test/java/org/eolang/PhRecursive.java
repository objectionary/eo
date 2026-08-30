/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * An object whose φ dataizes its own rho, until the given count runs out.
 *
 * <p>The recursion goes through the cache of the φ, and not through a new
 * object, so it shows what a memoized attribute does when it asks for the
 * very object it belongs to.</p>
 *
 * @since 0.1.0
 */
final class PhRecursive extends PhDefault {

    /**
     * Make one, with its φ in place.
     *
     * <p>The φ is attached here, and not in a constructor, because it is
     * an expression over the object itself, which does not exist yet
     * while its constructor runs.</p>
     *
     * @param count How many times the recursion may go deeper
     * @return The object
     */
    static Phi made(final AtomicInteger count) {
        final PhRecursive made = new PhRecursive();
        made.add(
            "φ",
            new AtComposite(
                made,
                rho -> {
                    final Phi result;
                    if (count.decrementAndGet() <= 0) {
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
