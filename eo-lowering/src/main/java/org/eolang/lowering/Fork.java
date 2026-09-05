/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * A choice between two nested protocols, as a step of a protocol.
 *
 * <p>It is what an {@code if} parked on a symbolic bool turns into: the
 * key of that bool, and one protocol per arm, each reduced on its own
 * from the argument the site held. The steps of an arm are computed only
 * when the arm is taken, and so are the reads of the voids that arm
 * alone touches, which is what keeps a guard guarding: an operation that
 * is partial, and an argument whose dataization may never end, both
 * stay behind the bool that protects them. The value of the fork is
 * whatever the taken arm answers, so the two arms
 * must answer the same forma, and a fork whose arms disagree refuses to
 * name one.</p>
 *
 * @since 0.76.0
 */
public final class Fork implements Step {

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
     * @param label The name of the step, such as {@code s2}
     * @param atom The λ name of the atom that parked
     * @param test The key of the bool that decides
     * @param yes The arm taken when the bool holds
     * @param not The arm taken otherwise
     */
    public Fork(final String label, final String atom, final String test,
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
        final String out = this.taken.carrier();
        if (!out.equals(this.other.carrier())) {
            throw new IllegalStateException(
                String.format(
                    "The fork '%s' answers a %s in one arm and a %s in the other",
                    this.name, out, this.other.carrier()
                )
            );
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
