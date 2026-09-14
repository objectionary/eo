/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * One entry into another fragment, as a step of a protocol.
 *
 * <p>It takes the name of the step, the locator of the formation being
 * entered with the arguments it binds, the keys of those arguments with
 * the receiver first, and the forma of the answer, and it answers those
 * four. The body behind the boundary is lowered by a run of its own, so at
 * run time this step goes through the object.</p>
 *
 * @since 0.77.0
 */
final class Entry implements Step {

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
    Entry(final String label, final String atom, final List<String> keys,
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
