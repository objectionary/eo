/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;

/**
 * One step of a protocol.
 *
 * <p>It is one value run time must compute, named so that the steps
 * after it and the answer can refer to it by the key {@code sym:<label>}.
 * An {@link Application} is one atom applied to operands that are
 * already values; a {@link Fork} picks between two nested protocols by a
 * bool that is already a value. Either way a step reads only keys minted
 * before it — a void of the fragment, an earlier step, or a literal with
 * its forma and bytes — so a protocol is a static single-assignment
 * program over the values the fragment starts from, with a block of its
 * own under every arm of every fork.</p>
 *
 * @since 0.76.0
 */
public interface Step {

    /**
     * The name of the step.
     * @return The name, such as {@code s1}
     */
    String label();

    /**
     * The λ name of the atom that parked into this step.
     * @return The name, such as {@code L_number_plus} or {@code L_bool_if}
     */
    String atom();

    /**
     * The forma of the value this step computes.
     * @return One of {@code number}, {@code bool}, {@code bytes}, {@code string}
     */
    String forma();

    /**
     * The keys of the values this step reads directly.
     * @return The receiver first and then the arguments, or the one
     *  condition of a fork
     */
    List<String> keys();

    /**
     * The protocols nested in this step.
     * @return The two arms of a fork, the taken one first; none for an application
     */
    List<Protocol> branches();
}
