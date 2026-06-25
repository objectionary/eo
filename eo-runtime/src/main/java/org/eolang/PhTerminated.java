/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The ⊥ ("bottom") object of φ-calculus — a terminated computation.
 *
 * <p>It carries no data and no attributes. Dataizing it aborts the
 * program with an {@link ExFailure}, which EO {@code try} cannot catch
 * (that object only intercepts {@link EOerror.ExError}), so reaching ⊥
 * terminates the program for good.</p>
 *
 * @since 0.73.1
 */
public final class PhTerminated extends PhDefault {

    @Override
    public byte[] delta() {
        throw new ExFailure("the ⊥ object is a terminated computation and cannot be dataized");
    }
}
