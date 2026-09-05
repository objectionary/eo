/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Symbolic evaluation of one XMIR fragment, into a protocol.
 *
 * <p>The fragment is a tree of applications over literals and voids. The
 * voids become symbolic carriers, and phino morphs the whole expression
 * under {@code --partial}: every application decided by data fires for
 * real, every application reaching a symbol parks, and each lands in the
 * evaluation records. The loop reads the records back into the tree — a
 * fired site folds into its literal value, a parked site becomes a fresh
 * symbol and one step of the protocol — and morphs again, until the tree
 * is a single value. Everything semantic happens inside phino; this side
 * only matches records to sites, by the shapes {@link Operand} anchors
 * and the one {@link Op} table of lowerable operations.</p>
 *
 * <p>Whatever does not settle is refused with an exception, never
 * repaired: a foreign atom, a site the records cannot anchor, a value of
 * a forma no carrier stands for, an exhausted budget. The caller treats
 * every refusal as one fragment staying unlowered, the way {@link Constant}
 * refusals are treated, so a refusal here is a filter, not a
 * failure.</p>
 *
 * @since 0.76.0
 * @todo #8308:30min Let a string literal stand as the receiver of a step.
 *  A record shows its receiver as the instance the atom fired on, and a
 *  string has already dispatched into its own bytes by then, so the
 *  shape names a bytes datum while the tree still holds a string one,
 *  the two never match, and {@code "abc".concat b} refuses where
 *  {@code b.concat "abc"} reduces. Teach {@link Shape} that a bytes
 *  receiver covers a string one carrying the same datum — the two
 *  compute the same value for every atom of the {@link Universe}, since
 *  the only method a string answers differently is shadowed there.
 */
public final class Reduction {

    /**
     * The binary that dataizes.
     */
    private final Phino phino;

    /**
     * The XMIR fragment to reduce, an {@code <o/>} element.
     */
    private final Xnav fragment;

    /**
     * The voids of the fragment: names to formas, in declaration order.
     */
    private final Map<String, String> voids;

    /**
     * The most partial runs one reduction may take.
     */
    private final int rounds;

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param xmir The XMIR fragment to reduce, an {@code <o/>} element
     * @param inputs The voids of the fragment: names to formas, in order
     * @param budget The most partial runs one reduction may take
     */
    public Reduction(final Phino exe, final Xnav xmir,
        final Map<String, String> inputs, final int budget) {
        this.phino = exe;
        this.fragment = xmir;
        this.voids = inputs;
        this.rounds = budget;
    }

    /**
     * The protocol the fragment reduces to.
     * @return The steps, the answer, and its forma
     * @throws IOException If the binary cannot be run
     */
    public Protocol protocol() throws IOException {
        final List<Step> steps = new ArrayList<>(0);
        Term tree = new Parsed(this.fragment, this.voids).term();
        int round = 0;
        while (tree.key().isEmpty()) {
            if (round >= this.rounds) {
                throw new IllegalStateException(
                    String.format("The reduction did not settle in %d rounds", this.rounds)
                );
            }
            ++round;
            tree = this.grown(tree, steps);
        }
        return new Protocol(steps, tree.key(), this.carrier(tree.key(), steps));
    }

    private Term grown(final Term tree, final List<Step> steps) throws IOException {
        final Trace trace = this.phino.partial(
            new Universe().text(),
            String.format("⟦%n  φ ↦ %s%n⟧%n", tree.phi())
        );
        Term out = tree;
        int done = 0;
        String excuse = "no atom parked at any known operation";
        for (final Evaluation record : trace.records()) {
            if (record.name().startsWith("Sym_")) {
                continue;
            }
            final Op operation = new Op(record.name());
            if (!operation.listed()) {
                excuse = String.format(
                    "the atom '%s' is not among the lowerable operations", record.name()
                );
                continue;
            }
            final Optional<Shape> shape = Reduction.shaped(operation, record);
            if (!shape.isPresent() || !out.matches(shape.get())) {
                continue;
            }
            final Term swap;
            if (record.parked()) {
                final String label = String.format("s%d", steps.size() + 1);
                final List<String> keys = new ArrayList<>(1);
                keys.add(Reduction.self(operation, record));
                keys.addAll(Reduction.arguments(operation, record).get());
                steps.add(new Step(label, record.name(), keys));
                swap = new Symbol(label, operation.forma());
            } else {
                final String[] parts = new Operand(record.result()).key().split(":", 2);
                swap = new Literal(parts[0], parts[1]);
            }
            out = out.swapped(shape.get(), swap);
            ++done;
        }
        if (done == 0) {
            throw new IllegalStateException(
                String.format("The reduction is stuck: %s", excuse)
            );
        }
        return out;
    }

    private static Optional<Shape> shaped(final Op operation, final Evaluation record) {
        Optional<Shape> out = Optional.empty();
        final Optional<List<String>> keys = Reduction.arguments(operation, record);
        final boolean whole = keys.isPresent()
            && (record.parked() || new Operand(record.result()).anchored());
        if (whole) {
            out = Optional.of(
                new Shape(
                    operation.method(),
                    Reduction.self(operation, record),
                    operation.args(),
                    keys.get()
                )
            );
        }
        return out;
    }

    private static Optional<List<String>> arguments(final Op operation, final Evaluation record) {
        final Map<String, String> bindings = record.bindings();
        final List<String> names = operation.args();
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

    private static String self(final Op operation, final Evaluation record) {
        final String found = record.receiver();
        final String out;
        if (found.startsWith("Δ:")) {
            out = String.format("%s:%s", operation.carrier(), found.substring(2));
        } else {
            out = found;
        }
        return out;
    }

    private String carrier(final String key, final List<Step> steps) {
        final String out;
        if (key.startsWith("sym:s")) {
            final String label = key.substring(4);
            String atom = "";
            for (final Step step : steps) {
                if (step.label().equals(label)) {
                    atom = step.atom();
                    break;
                }
            }
            if (atom.isEmpty()) {
                throw new IllegalStateException(
                    String.format("The answer '%s' names no step", key)
                );
            }
            out = new Op(atom).forma();
        } else if (key.startsWith("sym:v")) {
            out = new ArrayList<>(this.voids.values())
                .get(Integer.parseInt(key.substring(5)));
        } else {
            out = key.split(":", 2)[0];
        }
        return out;
    }
}
