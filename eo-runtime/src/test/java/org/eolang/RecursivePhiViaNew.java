/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * An object whose φ dataizes a freshly made copy of itself, until the
 * counter runs out.
 *
 * @since 0.1.0
 */
final class RecursivePhiViaNew extends PhDefault {

    /**
     * How many more times to recurse.
     */
    static final AtomicInteger COUNT = new AtomicInteger();

    /**
     * Ctor.
     */
    RecursivePhiViaNew() {
        super(
            new Attrs(
                new Attr(
                    "φ",
                    new AtComposite(
                        new PhDefault(),
                        rho -> {
                            final Phi result;
                            if (RecursivePhiViaNew.COUNT.decrementAndGet() <= 0) {
                                result = new Data.ToPhi(0L);
                            } else {
                                result = new Data.ToPhi(
                                    new Dataized(new RecursivePhiViaNew()).asNumber()
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
