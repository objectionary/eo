/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.List;

/**
 * The protocol a tree settles into when it fails.
 *
 * <p>The reason of the terminator is reduced into the same list of
 * steps, since a failure evaluates it to make its message, and the key
 * it becomes is what the protocol fails with. The reason must settle
 * into a value, not into a call or another terminator, and that value
 * must be a string, or the bytes one is made of, since the message of
 * the failure is the reason dataized and read as text: a reason of any
 * other forma would abort with a message the atom cannot spell, so it
 * is refused.</p>
 *
 * @since 0.76.0
 */
public final class Failure {

    /**
     * The reduction settling the reason.
     */
    private final Reduction reduction;

    /**
     * The ledger naming the carrier of the reason.
     */
    private final Minted ledger;

    /**
     * Ctor.
     * @param core The reduction settling the reason
     * @param minted The ledger naming the carrier of the reason
     */
    public Failure(final Reduction core, final Minted minted) {
        this.reduction = core;
        this.ledger = minted;
    }

    /**
     * The protocol of the failure.
     * @param fail The terminator the tree is
     * @param steps The steps of the protocol so far, to add to
     * @return The protocol, ending in the failure
     * @throws IOException If the binary cannot be run
     */
    public Protocol protocol(final Fail fail, final List<Step> steps) throws IOException {
        final Term reason = this.reduction.reduced(fail.reason(), steps, this.ledger);
        if (reason.key().isEmpty()) {
            throw new IllegalStateException(
                "The reason of the terminator must settle into a value, but it repeats or fails"
            );
        }
        final String forma = this.ledger.carried(reason);
        if (!"string".equals(forma) && !"bytes".equals(forma)) {
            throw new IllegalStateException(
                String.format(
                    "The reason of the terminator must be a string, but '%s' carries a %s",
                    reason.key(), forma
                )
            );
        }
        return new Protocol(steps, reason.key());
    }
}
