/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The ledger of the labels and formas one reduction shares.
 *
 * <p>Every loop of one reduction — the top of it, the arms of its forks,
 * the arguments of its repeats, the bodies of its recursive helpers —
 * shares one ledger, so that a label never repeats across the arms and
 * the forma of any step can be looked up wherever its symbol ends up. A
 * label is taken before the step is finished, since the arms of a fork
 * mint their own steps before the fork knows what forma it answers, and
 * it is bound to a forma once that is known. The ledger also knows the
 * voids: those of the formation first, and those of every body a repeat
 * resumes after them, declared with the formas of the values the first
 * repeat hands over, so it names the carrier of any key — a step by its
 * binding, a void by its position, a literal by its own prefix.</p>
 *
 * @since 0.76.0
 */
public final class Minted {

    /**
     * The formas of all the voids, by position.
     */
    private final List<String> voids;

    /**
     * The formas of the steps, by label, in the order they were taken.
     */
    private final Map<String, String> formas;

    /**
     * The voids of every body: names to the formas of their voids, the
     * formation itself under the empty name.
     */
    private final Map<String, List<String>> bodies;

    /**
     * The position of the first void of every body, by name.
     */
    private final Map<String, Integer> offsets;

    /**
     * Ctor.
     * @param inputs The voids of the fragment: names to formas, in order
     */
    public Minted(final Map<String, String> inputs) {
        this(
            new ArrayList<>(inputs.values()),
            new LinkedHashMap<>(0),
            Minted.first(new ArrayList<>(inputs.values())),
            Minted.first(0)
        );
    }

    private Minted(final List<String> kinds, final Map<String, String> labels,
        final Map<String, List<String>> parts, final Map<String, Integer> starts) {
        this.voids = kinds;
        this.formas = labels;
        this.bodies = parts;
        this.offsets = starts;
    }

    /**
     * Take the next label, bound to no forma yet.
     * @return The label, such as {@code s3}
     */
    public String next() {
        final String label = String.format("s%d", this.formas.size() + 1);
        this.formas.put(label, "");
        return label;
    }

    /**
     * Bind a label to the forma of its step.
     * @param label The label
     * @param forma The forma
     */
    public void bind(final String label, final String forma) {
        this.formas.put(label, forma);
    }

    /**
     * Declare the voids of a body a repeat resumes.
     * @param name The name of the helper
     * @param kinds The formas of its voids, in declaration order
     */
    public void declare(final String name, final List<String> kinds) {
        if (this.bodies.containsKey(name)) {
            throw new IllegalStateException(
                String.format("The body '%s' is declared already", name)
            );
        }
        this.offsets.put(name, this.voids.size());
        this.bodies.put(name, new ArrayList<>(kinds));
        this.voids.addAll(kinds);
    }

    /**
     * Whether the voids of a body are declared.
     * @param name The name of the helper, empty for the formation itself
     * @return True if declared
     */
    public boolean declared(final String name) {
        return this.bodies.containsKey(name);
    }

    /**
     * The names of the bodies declared, the formation itself first.
     * @return The names, in the order they were declared
     */
    public Collection<String> names() {
        return Collections.unmodifiableCollection(this.bodies.keySet());
    }

    /**
     * The formas of the voids of a body.
     * @param name The name of the helper, empty for the formation itself
     * @return The formas, in declaration order
     */
    public List<String> voids(final String name) {
        if (!this.bodies.containsKey(name)) {
            throw new IllegalStateException(
                String.format("The body '%s' is not declared", name)
            );
        }
        return Collections.unmodifiableList(this.bodies.get(name));
    }

    /**
     * The position of the first void of a body among all voids.
     * @param name The name of the helper, empty for the formation itself
     * @return The position
     */
    public int offset(final String name) {
        this.voids(name);
        return this.offsets.get(name);
    }

    /**
     * The carrier of a key.
     * @param key The key, such as {@code sym:s2}, {@code sym:v0} or {@code bool:FF-}
     * @return The forma, such as {@code number}
     */
    public String carrier(final String key) {
        final String out;
        if (key.startsWith("sym:s")) {
            out = this.formas.getOrDefault(key.substring(4), "");
            if (out.isEmpty()) {
                throw new IllegalStateException(
                    String.format("The key '%s' names no finished step", key)
                );
            }
        } else if (key.startsWith("sym:v")) {
            final int idx = Integer.parseInt(key.substring(5));
            if (idx >= this.voids.size()) {
                throw new IllegalStateException(
                    String.format("The key '%s' names no declared void", key)
                );
            }
            out = this.voids.get(idx);
        } else {
            out = key.split(":", 2)[0];
        }
        return out;
    }

    /**
     * The forma a settled term hands over, checked against the ledger.
     *
     * <p>A term keyed by a symbol stands for the value of that symbol,
     * and a {@link Forced} view of it stands for the bytes of the same
     * value under the same key, so the two agree on the key and differ
     * on the forma. The fragment settles into the bytes all the same,
     * and the atom renders the local through its raw bits.</p>
     *
     * @param tree The settled term, with a key
     * @return The forma the ledger holds for the key
     */
    public String carried(final Term tree) {
        final String key = tree.key();
        final String carrier = this.carrier(key);
        final String out;
        if (tree.forma().equals(carrier)) {
            out = carrier;
        } else if ("bytes".equals(tree.forma())) {
            out = "bytes";
        } else {
            throw new IllegalStateException(
                String.join(
                    " ",
                    String.format("The value '%s' carries a %s,", key, carrier),
                    String.format("but the fragment settles into its %s,", tree.forma()),
                    "which no view of it explains"
                )
            );
        }
        return out;
    }

    private static <T> Map<String, T> first(final T value) {
        final Map<String, T> out = new LinkedHashMap<>(1);
        out.put("", value);
        return out;
    }
}
