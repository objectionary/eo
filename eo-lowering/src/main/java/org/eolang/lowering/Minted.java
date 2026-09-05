/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The ledger of the steps one reduction has minted so far.
 *
 * <p>Every loop of one reduction — the top of it, the arms of its forks,
 * the arguments of its repeats — shares one ledger, so that a label
 * never repeats across the arms and the forma of any step can be looked
 * up wherever its symbol ends up. A label is taken before the step is
 * finished, since the arms of a fork mint their own steps before the
 * fork knows what forma it answers, and it is bound to a forma once
 * that is known. The ledger also knows the voids of the fragment, so it
 * names the carrier of any key: a step by its binding, a void by its
 * declaration, a literal by its own prefix.</p>
 *
 * @since 0.76.0
 */
public final class Minted {

    /**
     * The voids of the fragment: names to formas, in declaration order.
     */
    private final Map<String, String> voids;

    /**
     * The formas of the steps, by label, in the order they were taken.
     */
    private final Map<String, String> formas;

    /**
     * Ctor.
     * @param inputs The voids of the fragment: names to formas, in order
     */
    public Minted(final Map<String, String> inputs) {
        this.voids = inputs;
        this.formas = new LinkedHashMap<>(0);
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
            out = new ArrayList<>(this.voids.values())
                .get(Integer.parseInt(key.substring(5)));
        } else {
            out = key.split(":", 2)[0];
        }
        return out;
    }
}
