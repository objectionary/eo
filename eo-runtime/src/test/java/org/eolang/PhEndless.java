/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * An object whose φ is a copy of itself, until the given count runs out.
 *
 * <p>The count is handed in rather than kept in a static, so that two
 * dataizations of the object never share the depth they are allowed.</p>
 *
 * @since 0.1.0
 */
final class PhEndless extends PhDefault {

    /**
     * Ctor.
     * @param count How many times the recursion may go deeper
     */
    PhEndless(final AtomicInteger count) {
        super(
            new Attrs(
                new Attr(
                    Phi.PHI,
                    new AtComposite(
                        new PhDefault(),
                        self -> {
                            final Phi result;
                            if (count.decrementAndGet() <= 0) {
                                result = new Data.ToPhi(0L);
                            } else {
                                result = new PhEndless(count).copy();
                            }
                            return result;
                        }
                    )
                )
            )
        );
    }
}
