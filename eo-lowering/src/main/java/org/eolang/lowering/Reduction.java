/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
 * <p>A call of the formation to itself is the one term that never goes
 * to phino: it is a repeat, and φ has no expression for one. When such
 * a call is the root of the tree being settled, its arguments are
 * reduced in turn into the same list of steps — a repeat evaluates all
 * of them, there is nothing lazy about it — and the tree settles into
 * the keys they become, one per void, instead of an answer. When the
 * call stands anywhere else, phino parks on its marker and the
 * fragment is refused: the recursion is not in a tail position, and a
 * fork whose arm repeats is held to the same rule, since a repeat below
 * an operation that still awaits the value would rerun the whole body
 * in its place.</p>
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
     * The name of the formation the fragment is the body of, or empty.
     */
    private final String formation;

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param xmir The XMIR fragment to reduce, an {@code <o/>} element
     * @param inputs The voids of the fragment: names to formas, in order
     * @param budget The most partial runs one reduction may take
     */
    public Reduction(final Phino exe, final Xnav xmir,
        final Map<String, String> inputs, final int budget) {
        this(exe, xmir, inputs, budget, "");
    }

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param xmir The XMIR fragment to reduce, an {@code <o/>} element
     * @param inputs The voids of the fragment: names to formas, in order
     * @param budget The most partial runs one reduction may take
     * @param name The name of the formation the fragment is the body of,
     *  whose calls to itself become repeats; empty when there is none
     */
    public Reduction(final Phino exe, final Xnav xmir,
        final Map<String, String> inputs, final int budget, final String name) {
        this.phino = exe;
        this.fragment = xmir;
        this.voids = inputs;
        this.rounds = budget;
        this.formation = name;
    }

    /**
     * The protocol the fragment reduces to.
     * @return The steps, the answer, and its forma
     * @throws IOException If the binary cannot be run
     */
    public Protocol protocol() throws IOException {
        return this.settled(
            new Parsed(this.fragment, this.voids, this.formation).term(),
            new Minted(this.voids)
        );
    }

    private Protocol settled(final Term start, final Minted minted) throws IOException {
        final List<Step> steps = new ArrayList<>(0);
        final Term tree = this.reduced(start, steps, minted);
        final Optional<List<Term>> again = tree.again();
        final Protocol out;
        if (again.isPresent()) {
            out = new Protocol(steps, this.repeated(again.get(), steps, minted));
        } else {
            out = new Protocol(steps, tree.key(), minted.carrier(tree.key()));
        }
        return out;
    }

    private Term reduced(final Term start, final List<Step> steps, final Minted minted)
        throws IOException {
        Term tree = start;
        int round = 0;
        while (tree.key().isEmpty() && !tree.again().isPresent()) {
            if (round >= this.rounds) {
                throw new IllegalStateException(
                    String.format("The reduction did not settle in %d rounds", this.rounds)
                );
            }
            ++round;
            tree = this.grown(tree, steps, minted);
        }
        return tree;
    }

    private List<String> repeated(final List<Term> args, final List<Step> steps,
        final Minted minted) throws IOException {
        final List<String> formas = new ArrayList<>(this.voids.values());
        if (args.size() != formas.size()) {
            throw new IllegalStateException(
                String.format(
                    "The call to itself passes %d arguments to %d voids",
                    args.size(), formas.size()
                )
            );
        }
        final List<String> keys = new ArrayList<>(args.size());
        for (int idx = 0; idx < args.size(); ++idx) {
            final String key = this.reduced(args.get(idx), steps, minted).key();
            if (key.isEmpty()) {
                throw new IllegalStateException(
                    "A call to itself cannot be an argument of a call to itself"
                );
            }
            final String forma = minted.carrier(key);
            if (!formas.get(idx).equals(forma)) {
                throw new IllegalStateException(
                    String.format(
                        "The call to itself passes a %s where void #%d carries a %s",
                        forma, idx, formas.get(idx)
                    )
                );
            }
            keys.add(key);
        }
        return keys;
    }

    private Term grown(final Term tree, final List<Step> steps, final Minted minted)
        throws IOException {
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
            if ("L_self".equals(record.name())) {
                throw new IllegalStateException(
                    "The call to itself is not in a tail position, so the fragment cannot repeat"
                );
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
        final Evaluation record, final List<Step> steps, final Minted minted) {
        Optional<Term> out = Optional.empty();
        final Anchored anchored = new Anchored(operation, record);
        final Optional<Shape> shape = anchored.shape();
        if (shape.isPresent() && tree.matches(shape.get())) {
            final Term swap;
            if (record.parked()) {
                final String label = minted.next();
                minted.bind(label, operation.forma());
                final List<String> keys = new ArrayList<>(1);
                keys.add(anchored.receiver());
                keys.addAll(anchored.arguments().get());
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
        final Evaluation record, final List<Step> steps, final Minted minted)
        throws IOException {
        final String test = new Anchored(operation, record).receiver();
        final Optional<List<Binding>> found = tree.arguments(
            new Shape(
                operation.method(), test, operation.args(),
                Collections.nCopies(operation.args().size(), "")
            )
        );
        Optional<Term> out = Optional.empty();
        if (found.isPresent()) {
            final List<Binding> args = found.get();
            final Term swap;
            if ("bool:FF-".equals(test)) {
                swap = args.get(0).value();
            } else if ("bool:00-".equals(test)) {
                swap = args.get(1).value();
            } else {
                final String label = minted.next();
                final Step fork = new Fork(
                    label, record.name(), test,
                    this.settled(args.get(0).value(), minted),
                    this.settled(args.get(1).value(), minted)
                );
                minted.bind(label, fork.forma());
                steps.add(fork);
                swap = new Symbol(label, fork.forma());
            }
            final Term next = tree.swapped(new Shape(operation.method(), test, args), swap);
            if (Reduction.repeating(swap, steps) && !next.key().equals(swap.key())) {
                throw new IllegalStateException(
                    "The fork repeats in one arm but is not in a tail position itself"
                );
            }
            out = Optional.of(next);
        }
        return out;
    }

    private static boolean repeating(final Term swap, final List<Step> steps) {
        boolean out = false;
        for (final Step step : steps) {
            if (swap.key().equals(String.format("sym:%s", step.label()))) {
                out = step.branches().stream().anyMatch(Protocol::repeats);
                break;
            }
        }
        return out;
    }
}
