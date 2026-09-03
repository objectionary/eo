/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * One step of a protocol.
 *
 * <p>It is a single application whose value run time must compute: the λ
 * name of the atom, and the keys of its operands — the receiver first,
 * then the arguments in their positional order. An operand key names a
 * void of the fragment, an earlier step, or a literal with its forma and
 * bytes, so the whole protocol is a static single-assignment program
 * over the values the fragment starts from.</p>
 *
 * @since 0.76.0
 */
public final class Step {

    /**
     * The name of the step, such as {@code s1}.
     */
    private final String name;

    /**
     * The λ name of the atom, such as {@code L_number_plus}.
     */
    private final String lambda;

    /**
     * The keys of the operands: the receiver first, then the arguments.
     */
    private final List<String> operands;

    /**
     * Ctor.
     * @param label The name of the step, such as {@code s1}
     * @param atom The λ name of the atom, such as {@code L_number_plus}
     * @param keys The keys of the operands, the receiver first
     */
    public Step(final String label, final String atom, final List<String> keys) {
        this.name = label;
        this.lambda = atom;
        this.operands = keys;
    }

    /**
     * The name of the step.
     * @return The name, such as {@code s1}
     */
    public String label() {
        return this.name;
    }

    /**
     * The λ name of the atom.
     * @return The name, such as {@code L_number_plus}
     */
    public String atom() {
        return this.lambda;
    }

    /**
     * The keys of the operands.
     * @return The receiver first, then the arguments in positional order
     */
    public List<String> keys() {
        return Collections.unmodifiableList(this.operands);
    }
}
