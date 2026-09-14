/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;

/**
 * One step of a protocol: one value run time must compute.
 *
 * <p>It answers its own name, so that later steps reach it by the key
 * {@code sym:<label>}, the atom or method behind it, the forma of its
 * value, the keys it reads and the protocols nested under it. An
 * {@link Application} is a Java operator, a {@link Dispatch} is a call
 * back into EO, an {@link Entry} enters another fragment and a
 * {@link Fork} picks between two arms.</p>
 *
 * @since 0.76.0
 */
interface Step {

    /**
     * The name of the step.
     *
     * @return The name, such as {@code s1}
     */
    String label();

    /**
     * The λ name of the atom that parked into this step, or the method
     * of a dispatch back into EO, dot-prefixed.
     *
     * @return The name, such as {@code L_number_plus} or {@code L_bool_if},
     *  or a method such as {@code .minus}
     */
    String atom();

    /**
     * The forma of the value this step computes.
     *
     * @return One of {@code number}, {@code bool}, {@code bytes}, {@code string}
     */
    String forma();

    /**
     * The keys of the values this step reads directly.
     *
     * @return The receiver first and then the arguments, or the one
     *  condition of a fork
     */
    List<String> keys();

    /**
     * The protocols nested in this step.
     *
     * @return The two arms of a fork, the taken one first; none for an application
     */
    List<Protocol> branches();
}
