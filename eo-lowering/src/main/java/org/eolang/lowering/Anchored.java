/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * One evaluation record, anchored to the tree by keys.
 *
 * <p>A record shows the atom that fired or parked, its receiver as the
 * instance it was dispatched on, and its arguments as phi terms. Here
 * they become keys: the receiver by the datum or marker inside it,
 * prefixed with the carrier of the operation when it is a datum, and
 * each argument by the shape {@link Operand} anchors, read under the
 * name the row of the operation gives it or under the positional name
 * phino may still show. A record whose arguments do not all anchor, or
 * whose result does not, has no shape yet: the site it names holds a
 * value that is not reduced, and the loop waits for a later round. A
 * record with a shape rewrites the tree: the site it names gives way
 * to the literal a fired atom computed, or to the symbol of the step a
 * parked atom is minted into.</p>
 *
 * @since 0.76.0
 */
public final class Anchored {

    /**
     * The operation the record names.
     */
    private final Op operation;

    /**
     * The record.
     */
    private final Evaluation record;

    /**
     * Ctor.
     * @param atom The operation the record names
     * @param rec The record
     */
    public Anchored(final Op atom, final Evaluation rec) {
        this.operation = atom;
        this.record = rec;
    }

    /**
     * The key of the receiver.
     * @return A key such as {@code sym:v0} or {@code number:40-...}
     */
    public String receiver() {
        final String found = this.record.receiver();
        final String out;
        if (found.startsWith("Δ:")) {
            out = String.format("%s:%s", this.operation.carrier(), found.substring(2));
        } else {
            out = found;
        }
        return out;
    }

    /**
     * The keys of the arguments, in their positional order.
     * @return The keys, or empty when any argument is not anchored yet
     */
    public Optional<List<String>> arguments() {
        final Map<String, String> bindings = this.record.bindings();
        final List<String> names = this.operation.args();
        final List<String> keys = new ArrayList<>(names.size());
        boolean good = bindings.size() == names.size();
        for (int idx = 0; good && idx < names.size(); ++idx) {
            final Operand operand = new Operand(
                Objects.toString(
                    bindings.getOrDefault(
                        names.get(idx),
                        bindings.get(String.format("α%d", idx))
                    ),
                    ""
                )
            );
            good = operand.anchored();
            if (good) {
                keys.add(operand.key());
            }
        }
        Optional<List<String>> out = Optional.empty();
        if (good) {
            out = Optional.of(keys);
        }
        return out;
    }

    /**
     * The tree with the site of the record replaced by what the record
     * proves: the literal a fired atom computed, or the symbol of the
     * step a parked atom is minted into.
     * @param tree The tree
     * @param steps The steps of the protocol so far, to add to
     * @param minted The ledger the reduction shares
     * @return The rewritten tree, or empty when the record is not
     *  anchored yet or no site of the tree is the one recorded
     */
    public Optional<Term> applied(final Term tree, final List<Step> steps,
        final Minted minted) {
        Optional<Term> out = Optional.empty();
        final Optional<Shape> shape = this.shape();
        if (shape.isPresent() && tree.matches(shape.get())) {
            final Term swap;
            if (this.record.parked()) {
                final String label = minted.next();
                minted.bind(label, this.operation.forma());
                final List<String> keys = new ArrayList<>(1);
                keys.add(this.receiver());
                keys.addAll(this.arguments().get());
                steps.add(new Application(label, this.record.name(), keys));
                swap = new Symbol(label, this.operation.forma());
            } else {
                final String[] parts = new Operand(this.record.result()).key().split(":", 2);
                swap = new Literal(parts[0], parts[1]);
            }
            out = Optional.of(tree.swapped(shape.get(), swap));
        }
        return out;
    }

    /**
     * The shape of the site the atom fired or parked at.
     * @return The shape, or empty when the record is not anchored yet
     */
    public Optional<Shape> shape() {
        Optional<Shape> out = Optional.empty();
        final Optional<List<String>> keys = this.arguments();
        final boolean whole = keys.isPresent()
            && (this.record.parked() || new Operand(this.record.result()).anchored());
        if (whole) {
            out = Optional.of(
                new Shape(
                    this.operation.method(),
                    this.receiver(),
                    this.operation.args(),
                    keys.get()
                )
            );
        }
        return out;
    }
}
