/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * A step that enters another fragment: the formation at a locator is
 * applied to computed values and asked for what it answers, which the
 * atom does through the object at run time, since the body behind the
 * boundary is lowered by a run of its own.
 *
 * <p>The atom of the step is the locator followed by the names of the
 * voids the step binds, in parentheses, such as
 * {@code Φ.demo.walk(i,acc)}. The first key is the receiver: the
 * formation itself when the fragment reaches it lexically, handed to the
 * atom as an input of forma {@code formation}, or the value the
 * formation is dispatched on when it stands on a carrier.</p>
 *
 * @since 0.77.0
 */
public final class Entry implements Step {

    /**
     * The label.
     */
    private final String name;

    /**
     * The locator with the names of the bound voids.
     */
    private final String site;

    /**
     * The receiver and the operands.
     */
    private final List<String> operands;

    /**
     * What the formation answers.
     */
    private final String carrier;

    /**
     * Ctor.
     *
     * @param label The label
     * @param atom The locator with the names of the bound voids
     * @param keys The receiver and the operands
     * @param forma What the formation answers
     */
    public Entry(final String label, final String atom, final List<String> keys,
        final String forma) {
        this.name = label;
        this.site = atom;
        this.operands = keys;
        this.carrier = forma;
    }

    @Override
    public String label() {
        return this.name;
    }

    @Override
    public String atom() {
        return this.site;
    }

    @Override
    public String forma() {
        return this.carrier;
    }

    @Override
    public List<String> keys() {
        return Collections.unmodifiableList(this.operands);
    }

    @Override
    public List<Protocol> branches() {
        return Collections.emptyList();
    }
}
