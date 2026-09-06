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
 * <p>A call of the formation to itself, or of a recursive helper of it,
 * is the one term that never goes to phino: it is a repeat, and φ has
 * no expression for one. When such a call is the root of the tree being
 * settled, its arguments are reduced in turn into the same list of
 * steps — a repeat evaluates all of them, there is nothing lazy about
 * it — and the tree settles into the body it resumes and the keys its
 * voids take, one per void, instead of an answer; the first repeat into
 * a helper declares the formas of the helper's voids, and every later
 * one must agree. When the call stands anywhere else, phino parks on
 * its marker and the fragment is refused: the recursion is not in a
 * tail position, and a fork whose arm repeats is held to the same rule,
 * since a repeat below an operation that still awaits the value would
 * rerun the whole body in its place. The keys a repeat hands over, and
 * the formas it declares or checks, are the concern of {@link Repeat};
 * the bodies a program is made of, and the order they are reduced in,
 * of {@link Bodies}.</p>
 *
 * <p>A helper the formation binds next to its body is read in place
 * wherever the body names it, by {@link Parsed}, applied to its
 * arguments when it has voids of its own, so the tree phino sees
 * is the one the body would be with every helper written out, and the
 * protocol is the one that body would give: a helper named twice
 * stands twice and costs one step, since identical sites collapse, and
 * a helper named in one arm of a fork alone is computed in that arm
 * alone.</p>
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
     * The helpers the formation binds next to the fragment: names to
     * their {@code <o/>} elements.
     */
    private final Map<String, Xnav> helpers;

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
        this(exe, xmir, inputs, budget, name, Collections.emptyMap());
    }

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param xmir The XMIR fragment to reduce, an {@code <o/>} element
     * @param inputs The voids of the fragment: names to formas, in order
     * @param budget The most partial runs one reduction may take
     * @param name The name of the formation the fragment is the body of,
     *  whose calls to itself become repeats; empty when there is none
     * @param bound The helpers the formation binds next to the fragment:
     *  names to their {@code <o/>} elements, read in place when named
     */
    public Reduction(final Phino exe, final Xnav xmir,
        final Map<String, String> inputs, final int budget, final String name,
        final Map<String, Xnav> bound) {
        this.phino = exe;
        this.fragment = xmir;
        this.voids = inputs;
        this.rounds = budget;
        this.formation = name;
        this.helpers = bound;
    }

    /**
     * The protocol the fragment reduces to, when it is one body.
     * @return The steps, the answer, and its forma
     * @throws IOException If the binary cannot be run
     */
    public Protocol protocol() throws IOException {
        final Program program = this.program();
        if (program.bodies().size() > 1) {
            throw new IllegalStateException(
                "The fragment resumes a helper of the formation, which one protocol cannot express"
            );
        }
        return program.bodies().get(0).protocol();
    }

    /**
     * The program the fragment reduces to: its own body, and the body of
     * every recursive helper it resumes.
     * @return The bodies, the fragment's own first
     * @throws IOException If the binary cannot be run
     */
    public Program program() throws IOException {
        return new Bodies(
            this, this.fragment, this.voids, this.formation, this.helpers
        ).program();
    }

    /**
     * The protocol one tree settles into.
     * @param start The tree
     * @param minted The ledger this reduction shares
     * @return The protocol
     * @throws IOException If the binary cannot be run
     */
    Protocol settled(final Term start, final Minted minted) throws IOException {
        final List<Step> steps = new ArrayList<>(0);
        final Term tree = this.reduced(start, steps, minted);
        final Protocol out;
        if (tree.again().isPresent()) {
            out = new Repeat(this, minted).protocol(tree.again().get(), steps);
        } else {
            out = new Protocol(steps, tree.key(), minted.carried(tree));
        }
        return out;
    }

    /**
     * The value one tree settles into, over the steps it adds.
     * @param start The tree
     * @param steps The steps of the protocol being built
     * @param minted The ledger this reduction shares
     * @return A term with a key, or the call the tree is
     * @throws IOException If the binary cannot be run
     */
    Term reduced(final Term start, final List<Step> steps, final Minted minted)
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
