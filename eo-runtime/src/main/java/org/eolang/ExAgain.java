/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * A signal that a tail call of a looped formation was reached.
 *
 * <p>{@link PhAgain} throws it when the call it wraps is forced, and the
 * nearest {@link PhLoop} catches it and carries on with the object it holds,
 * instead of letting the Java stack grow by one more level per iteration.
 * It is control flow rather than an error, so it records no stack trace.
 * One that escapes every loop is a defect of the transpiler, and
 * {@link Main} reports it with the message set here.</p>
 *
 * @since 0.76
 */
public final class ExAgain extends ExAbstract {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = 597749420437007617L;

    /**
     * The next copy of the formation, the one the tail call made.
     */
    private final transient Phi next;

    /**
     * Ctor.
     *
     * @param phi The next copy of the formation
     */
    public ExAgain(final Phi phi) {
        super("A tail call was forced outside of its loop");
        this.next = phi;
    }

    /**
     * The next copy of the formation.
     *
     * @return The object the tail call made
     */
    public Phi next() {
        return this.next;
    }

    @Override
    public Throwable fillInStackTrace() {
        return this;
    }
}
