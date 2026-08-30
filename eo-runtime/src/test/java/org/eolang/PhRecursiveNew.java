/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * An object whose φ dataizes a fresh instance of the same class, until the
 * given count runs out.
 *
 * <p>Each step builds a new object instead of reading the cache of the old
 * one, so the recursion is stopped by the count alone and not by a
 * memoized attribute.</p>
 *
 * @since 0.1.0
 */
final class PhRecursiveNew extends PhDefault {

    /**
     * Ctor.
     * @param count How many times the recursion may go deeper
     */
    PhRecursiveNew(final AtomicInteger count) {
        super(
            new Attrs(
                new Attr(
                    "φ",
                    new AtComposite(
                        new PhDefault(),
                        rho -> {
                            final Phi result;
                            if (count.decrementAndGet() <= 0) {
                                result = new Data.ToPhi(0L);
                            } else {
                                result = new Data.ToPhi(
                                    new Dataized(new PhRecursiveNew(count)).asNumber()
                                );
                            }
                            return result;
                        }
                    )
                )
            )
        );
    }
}
