/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
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
 * <p>An {@code if} is the one operation whose arguments must not be
 * reduced in place: they are the arms of a choice, and a step minted
 * from either would compute regardless of the bool that guards it. So
 * when the {@code if} atom parks on a symbolic bool, the two arguments
 * are taken out of the site as they stand in the tree, each is reduced
 * by a loop of its own, and the site becomes a {@link Fork} holding one
 * protocol per arm. Every loop of one reduction shares the voids and one
 * ledger of the steps minted so far, by label, so that a label never
 * repeats across the arms and the forma of any step can be looked up
 * wherever its symbol ends up. When the bool is data instead, the site
 * simply gives way to the arm it picks.</p>
 *
 * <p>Whatever does not settle is refused with an exception, never
 * repaired: a foreign atom, a site the records cannot anchor, a value of
 * a forma no carrier stands for, an exhausted budget, an arm of a fork
 * that refuses or answers a forma the other arm does not. The caller
 * treats every refusal as one fragment staying unlowered, the way
 * {@link Constant} refusals are treated, so a refusal here is a filter,
 * not a failure.</p>
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
        return this.settled(
            new Parsed(this.fragment, this.voids).term(),
            new LinkedHashMap<>(0)
        );
    }

    private Protocol settled(final Term start, final Map<String, String> minted)
        throws IOException {
        final List<Step> steps = new ArrayList<>(0);
        Term tree = start;
        int round = 0;
        while (tree.key().isEmpty()) {
            if (round >= this.rounds) {
                throw new IllegalStateException(
                    String.format("The reduction did not settle in %d rounds", this.rounds)
                );
            }
            ++round;
            tree = this.grown(tree, steps, minted);
        }
        return new Protocol(steps, tree.key(), this.carrier(tree.key(), minted));
    }

    private Term grown(final Term tree, final List<Step> steps,
        final Map<String, String> minted) throws IOException {
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
            final Optional<Term> next;
            if (record.parked() && operation.forma().isEmpty()) {
                next = this.forked(out, operation, record, steps, minted);
            } else {
                next = Reduction.applied(out, operation, record, steps, minted);
            }
            if (next.isPresent()) {
                out = next.get();
                ++done;
            }
        }
        if (done == 0) {
            throw new IllegalStateException(
                String.format("The reduction is stuck: %s", excuse)
            );
        }
        return out;
    }

    private static Optional<Term> applied(final Term tree, final Op operation,
        final Evaluation record, final List<Step> steps, final Map<String, String> minted) {
        Optional<Term> out = Optional.empty();
        final Optional<Shape> shape = Reduction.shaped(operation, record);
        if (shape.isPresent() && tree.matches(shape.get())) {
            final Term swap;
            if (record.parked()) {
                final String label = String.format("s%d", minted.size() + 1);
                minted.put(label, operation.forma());
                final List<String> keys = new ArrayList<>(1);
                keys.add(Reduction.self(operation, record));
                keys.addAll(Reduction.arguments(operation, record).get());
                steps.add(new Application(label, record.name(), keys));
                swap = new Symbol(label, operation.forma());
            } else {
                final String[] parts = new Operand(record.result()).key().split(":", 2);
                swap = new Literal(parts[0], parts[1]);
            }
            out = Optional.of(tree.swapped(shape.get(), swap));
        }
        return out;
    }

    private Optional<Term> forked(final Term tree, final Op operation,
        final Evaluation record, final List<Step> steps, final Map<String, String> minted)
        throws IOException {
        final String self = Reduction.self(operation, record);
        final Optional<List<Binding>> found = tree.arguments(
            new Shape(
                operation.method(), self, operation.args(),
                Collections.nCopies(operation.args().size(), "")
            )
        );
        Optional<Term> out = Optional.empty();
        if (found.isPresent()) {
            final List<Binding> args = found.get();
            final Term swap;
            if ("bool:FF-".equals(self)) {
                swap = args.get(0).value();
            } else if ("bool:00-".equals(self)) {
                swap = args.get(1).value();
            } else {
                final String label = String.format("s%d", minted.size() + 1);
                minted.put(label, "");
                final Step fork = new Fork(
                    label, record.name(), self,
                    this.settled(args.get(0).value(), minted),
                    this.settled(args.get(1).value(), minted)
                );
                minted.put(label, fork.forma());
                steps.add(fork);
                swap = new Symbol(label, fork.forma());
            }
            out = Optional.of(tree.swapped(new Shape(operation.method(), self, args), swap));
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

    private String carrier(final String key, final Map<String, String> minted) {
        final String out;
        if (key.startsWith("sym:s")) {
            out = minted.getOrDefault(key.substring(4), "");
            if (out.isEmpty()) {
                throw new IllegalStateException(
                    String.format("The answer '%s' names no step", key)
                );
            }
        } else if (key.startsWith("sym:v")) {
            out = new ArrayList<>(this.voids.values())
                .get(Integer.parseInt(key.substring(5)));
        } else {
            out = key.split(":", 2)[0];
        }
        return out;
    }
}
