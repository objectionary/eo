/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Error that stops a computation running on an interrupted thread.
 *
 * <p>Dataization is a long chain of attribute lookups, where nothing ever
 * blocks. A thread that has to be stopped from the outside — a test that
 * outlived its deadline, a program that must shut down — therefore never
 * notices the interrupt and keeps computing forever, holding a CPU core
 * until the JVM dies. {@link PhDefault#take(String)} throws this error on
 * the very next lookup instead.</p>
 *
 * <p>The interrupt flag stays up when this error is thrown, so every next
 * lookup fails the same way and no {@code try} can resurrect the
 * computation.</p>
 *
 * @since 0.74.0
 */
public final class ExInterrupted extends ExAbstract {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = 597749420437007616L;

    /**
     * Ctor.
     *
     * @param cause Exception cause
     */
    public ExInterrupted(final String cause) {
        super(cause);
    }
}
