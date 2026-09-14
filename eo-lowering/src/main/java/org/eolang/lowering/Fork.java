/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * A choice between two protocols, as a step of a protocol.
 *
 * <p>It takes the key of a bool and one protocol per arm, and answers
 * those and the forma both arms agree on. Only the arm that is taken runs,
 * which is what keeps a guard guarding: the work an arm alone needs stays
 * behind the bool that protects it. Arms that disagree on a forma, or that
 * never answer at all, name none.</p>
 *
 * @since 0.76.0
 */
final class Fork implements Step {

    /**
     * The name of the step, such as {@code s2}.
     */
    private final String name;

    /**
     * The λ name of the atom that parked, such as {@code L_bool_if}.
     */
    private final String lambda;

    /**
     * The key of the bool that decides.
     */
    private final String condition;

    /**
     * The arm taken when the bool holds.
     */
    private final Protocol taken;

    /**
     * The arm taken otherwise.
     */
    private final Protocol other;

    /**
     * Ctor.
     *
     * @param label The name of the step, such as {@code s2}
     * @param atom The λ name of the atom that parked
     * @param test The key of the bool that decides
     * @param yes The arm taken when the bool holds
     * @param not The arm taken otherwise
     */
    Fork(final String label, final String atom, final String test,
        final Protocol yes, final Protocol not) {
        this.name = label;
        this.lambda = atom;
        this.condition = test;
        this.taken = yes;
        this.other = not;
    }

    @Override
    public String label() {
        return this.name;
    }

    @Override
    public String atom() {
        return this.lambda;
    }

    @Override
    public String forma() {
        final String yes = this.taken.carrier();
        final String not = this.other.carrier();
        if (!yes.isEmpty() && !not.isEmpty() && !yes.equals(not)) {
            throw new IllegalStateException(
                String.format(
                    "The fork '%s' answers a %s in one arm and a %s in the other",
                    this.name, yes, not
                )
            );
        }
        final String out;
        if (yes.isEmpty()) {
            out = not;
        } else {
            out = yes;
        }
        return out;
    }

    @Override
    public List<String> keys() {
        return Collections.singletonList(this.condition);
    }

    @Override
    public List<Protocol> branches() {
        return Arrays.asList(this.taken, this.other);
    }
}
