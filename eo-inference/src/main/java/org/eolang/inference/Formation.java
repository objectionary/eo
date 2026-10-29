/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A formation, as the objects part of {@code facts.xml} states it.
 *
 * <p>It has attributes, voids among them, in the order they are written;
 * the voids other than {@code ρ} are its voids in that order, the ones a
 * call fills one after the other. A formation may declare its {@code ρ}
 * ({@code [^]}) with the formation it holds, and an atom says which
 * formation Java returns, or which of its own voids it gives back.</p>
 *
 * <p>Which voids of a copy are bound is kept as bits, one per void in the
 * order of the formation and one more for {@code ρ}, so that a copy's
 * name is a number and two names compare at once.</p>
 *
 * @since 0.76.0
 */
final class Formation {

    /**
     * The bit of {@code ρ} among the bound voids.
     */
    static final long RHO = 1L << 62;

    /**
     * The attributes, by name, in the order they are written.
     */
    private final Map<String, Attribute> attrs;

    /**
     * The voids other than {@code ρ}, in the order they are written.
     */
    private final List<String> order;

    /**
     * What it says about its own {@code ρ} and what Java gives.
     */
    private final Map<String, String> said;

    /**
     * The types an atom calls each void back with, by void.
     */
    private final Map<String, List<String>> args;

    /**
     * The places an atom gives back, in the order they are written.
     */
    private final List<String> gives;

    /**
     * Ctor.
     *
     * @param attributes The attributes, by name, in the order written
     * @param voids The voids other than {@code ρ}, in the order written
     * @param notes What it says: "rho", "returns" and "complete", each
     *  empty when it says nothing
     * @param back The types an atom calls each void back with
     * @param places The places an atom gives back
     */
    Formation(
        final Map<String, Attribute> attributes, final List<String> voids,
        final Map<String, String> notes, final Map<String, List<String>> back,
        final List<String> places
    ) {
        this.attrs = attributes;
        this.order = voids;
        this.said = notes;
        this.args = back;
        this.gives = places;
    }

    /**
     * Whether it has an attribute of this name.
     *
     * @param name The name
     * @return TRUE if it has
     */
    boolean has(final String name) {
        return this.attrs.containsKey(name);
    }

    /**
     * Whether its attribute of this name is a void.
     *
     * @param name The name
     * @return TRUE if it is a void, FALSE if not or if there is none
     */
    boolean open(final String name) {
        return this.attrs.containsKey(name) && this.attrs.get(name).open();
    }

    /**
     * The formation its attribute of this name is declared to hold.
     *
     * @param name The name
     * @return The formation, empty when none is declared
     */
    String holds(final String name) {
        final String held;
        if (this.attrs.containsKey(name)) {
            held = this.attrs.get(name).holds();
        } else {
            held = "";
        }
        return held;
    }

    /**
     * The names of its attributes, in the order written.
     *
     * @return The names
     */
    Set<String> names() {
        return this.attrs.keySet();
    }

    /**
     * Its voids other than {@code ρ}, in the order written.
     *
     * @return The voids
     */
    List<String> order() {
        return this.order;
    }

    /**
     * The bit of one of its voids, {@code ρ} too.
     *
     * @param name The void
     * @return The bit, zero when it is not one of its voids
     */
    long bit(final String name) {
        final long bit;
        if ("ρ".equals(name)) {
            bit = Formation.RHO;
        } else {
            final int pos = this.order.indexOf(name);
            if (pos < 0) {
                bit = 0L;
            } else {
                bit = 1L << pos;
            }
        }
        return bit;
    }

    /**
     * The formation its declared {@code ρ} holds.
     *
     * @return The formation, empty when it declares none
     */
    String rho() {
        return this.said.getOrDefault("rho", "");
    }

    /**
     * The formation Java returns for it, if it is an atom.
     *
     * @return The formation, empty when it is not an atom
     */
    String returns() {
        return this.said.getOrDefault("returns", "");
    }

    /**
     * Whether it has every attribute it will ever have.
     *
     * @return TRUE if it has
     */
    boolean complete() {
        return "true".equals(this.said.get("complete"));
    }

    /**
     * Whether its atom calls a void back.
     *
     * @param name The void
     * @return TRUE if it does
     */
    boolean back(final String name) {
        return this.args.containsKey(name);
    }

    /**
     * The types its atom calls a void back with.
     *
     * @param name The void
     * @return The types, none when it does not call it back
     */
    List<String> args(final String name) {
        return this.args.getOrDefault(name, Collections.emptyList());
    }

    /**
     * The places its atom gives back.
     *
     * @return The places, in the order written
     */
    List<String> gives() {
        return this.gives;
    }
}
