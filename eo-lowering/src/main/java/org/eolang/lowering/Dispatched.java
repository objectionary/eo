/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The step a parked dispatch marker turns into.
 *
 * <p>A {@link Site} whose receiver is settled and whose method the
 * universe does not answer renders as a formation binding the receiver
 * to {@code self}, the method and the witnessed forma as data, and the
 * arguments to {@code a0}, {@code a1} and so on, next to the marker λ
 * {@code L_dispatch}; phino parks on it when, and only when, it demands
 * the value, so the record arrives in the round the value is needed,
 * the way any other parked atom arrives, and an arm never taken never
 * dispatches. The record names the receiver and the method, which is
 * enough to find the site in the tree; the arguments are then reduced
 * in turn into the same list of steps, since the call evaluates all of
 * them, and the site gives way to the symbol of a {@link Dispatch}
 * step. A method that is lazy in an argument — {@code if}, and the
 * {@code and} and {@code or} of a bool, which are an {@code if} — is
 * refused, since a strict call would compute what EO leaves alone.</p>
 *
 * @since 0.76.0
 */
public final class Dispatched {

    /**
     * The datum a marker slot carries.
     */
    private static final Pattern DATUM = Pattern.compile(
        "Δ ⤍ ([0-9A-F]{2}(?:-[0-9A-F]{2})*)-?"
    );

    /**
     * The methods lazy in an argument, which a strict call misreads.
     */
    private static final Collection<String> LAZY = new HashSet<>(
        Arrays.asList("if", "and", "or")
    );

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
     *
     * @param reduction The reduction settling each argument
     * @param ledger The ledger the reduction shares
     */
    public Dispatched(final Reduction reduction, final Minted ledger) {
        this.core = reduction;
        this.minted = ledger;
    }

    /**
     * The tree with the site of the record replaced by the step.
     *
     * @param tree The tree
     * @param record The record of the parked marker
     * @param steps The steps of the protocol so far, to add to
     * @return The rewritten tree, or empty when no site of the tree is
     *  the one recorded
     * @throws IOException If the binary cannot be run
     */
    public Optional<Term> applied(final Term tree, final Evaluation record,
        final List<Step> steps) throws IOException {
        final Map<String, String> bindings = record.bindings();
        final String method = Dispatched.decoded(bindings.getOrDefault("m🌵", ""));
        if (Dispatched.LAZY.contains(method)) {
            throw new IllegalStateException(
                String.format(
                    "The method '%s' is lazy in an argument, so it cannot be dispatched strictly",
                    method
                )
            );
        }
        final Operand self = new Operand(bindings.getOrDefault("self", ""));
        Optional<Term> out = Optional.empty();
        if (self.anchored()) {
            out = this.replaced(tree, self.key(), method, bindings, steps);
        }
        return out;
    }

    private Optional<Term> replaced(final Term tree, final String receiver,
        final String method, final Map<String, String> bindings,
        final List<Step> steps) throws IOException {
        final List<String> names = new ArrayList<>(0);
        final List<String> guesses = new ArrayList<>(0);
        for (int idx = 0; bindings.containsKey(String.format("a%d", idx)); ++idx) {
            names.add(String.format("α%d", idx));
            final Operand operand = new Operand(bindings.get(String.format("a%d", idx)));
            if (operand.anchored()) {
                guesses.add(operand.key());
            } else {
                guesses.add("");
            }
        }
        final Optional<List<Binding>> found = tree.arguments(
            new Shape(method, receiver, names, guesses)
        );
        Optional<Term> out = Optional.empty();
        if (found.isPresent()) {
            final List<Binding> args = found.get();
            final Shape exact = new Shape(method, receiver, args);
            final List<String> keys = new ArrayList<>(args.size() + 1);
            keys.add(receiver);
            for (final Binding arg : args) {
                final Term value = this.core.reduced(arg.value(), steps, this.minted);
                if (value.key().isEmpty()) {
                    throw new IllegalStateException(
                        String.format(
                            "An argument of the dispatch '%s' must settle into a value, not %s",
                            method, "repeat or fail"
                        )
                    );
                }
                keys.add(value.key());
            }
            final String forma = Dispatched.forma(bindings);
            final String label = this.minted.next();
            this.minted.bind(label, forma);
            steps.add(new Dispatch(label, method, keys, forma));
            out = Optional.of(tree.swapped(exact, new Symbol(label, forma)));
        }
        return out;
    }

    private static String forma(final Map<String, String> bindings) {
        String out = "object";
        if (bindings.containsKey("f🌵")) {
            out = Dispatched.decoded(bindings.get("f🌵"));
        }
        return out;
    }

    private static String decoded(final String term) {
        final Matcher found = Dispatched.DATUM.matcher(term);
        if (!found.find()) {
            throw new IllegalStateException(
                String.format("The marker slot '%s' carries no text", term)
            );
        }
        final String[] pairs = found.group(1).split("-", -1);
        final byte[] bytes = new byte[pairs.length];
        for (int idx = 0; idx < pairs.length; ++idx) {
            bytes[idx] = (byte) Integer.parseInt(pairs[idx], 16);
        }
        return new String(bytes, StandardCharsets.UTF_8);
    }
}
