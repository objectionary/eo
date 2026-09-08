/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * The protocol a tree settles into when it resumes a body.
 *
 * <p>The arguments of the call are reduced in turn into the same list of
 * steps, since a repeat evaluates all of them, and the keys they become
 * are what the voids of the resumed body take next. The first repeat
 * into a helper declares the formas of the helper's voids in the ledger,
 * with the formas of the values it hands over, and every repeat, into
 * the formation itself or into a helper declared already, must hand over
 * as many values as there are voids, each of the forma its void
 * carries.</p>
 *
 * @since 0.76.0
 */
public final class Repeat {

    /**
     * The reduction settling each argument.
     */
    private final Reduction core;

    /**
     * The ledger the reduction shares.
     */
    private final Minted minted;

    /**
     * Ctor.
     * @param reduction The reduction settling each argument
     * @param ledger The ledger the reduction shares
     */
    public Repeat(final Reduction reduction, final Minted ledger) {
        this.core = reduction;
        this.minted = ledger;
    }

    /**
     * The protocol of the repeat.
     * @param call The call that resumes a body
     * @param steps The steps of the protocol so far, to add to
     * @return The protocol, ending in the repeat
     * @throws IOException If the binary cannot be run
     */
    public Protocol protocol(final Again call, final List<Step> steps) throws IOException {
        final List<Term> args = call.arguments();
        final List<String> keys = new ArrayList<>(args.size());
        final List<String> handed = new ArrayList<>(args.size());
        for (final Term arg : args) {
            final Term value = this.core.reduced(arg, steps, this.minted);
            if (value.key().isEmpty()) {
                throw new IllegalStateException(
                    "A call to itself cannot be an argument of a call to itself"
                );
            }
            keys.add(value.key());
            handed.add(this.minted.carried(value));
        }
        if (!this.minted.declared(call.name())) {
            this.minted.declare(call.name(), handed);
        }
        final List<String> formas = this.minted.voids(call.name());
        if (handed.size() != formas.size()) {
            throw new IllegalStateException(
                String.format(
                    "The call to %s passes %d arguments to %d voids",
                    Repeat.whom(call), handed.size(), formas.size()
                )
            );
        }
        for (int idx = 0; idx < handed.size(); ++idx) {
            if (!formas.get(idx).equals(handed.get(idx))) {
                throw new IllegalStateException(
                    String.format(
                        "The call to %s passes a %s where void #%d carries a %s",
                        Repeat.whom(call), handed.get(idx), idx, formas.get(idx)
                    )
                );
            }
        }
        return new Protocol(steps, call.name(), keys);
    }

    private static String whom(final Again call) {
        final String out;
        if (call.name().isEmpty()) {
            out = "itself";
        } else {
            out = String.format("'ξ.%s'", call.name());
        }
        return out;
    }
}
