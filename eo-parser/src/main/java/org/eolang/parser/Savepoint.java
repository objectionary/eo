/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * Opaque token from {@link Emit#savepoint()}, restored by
 * {@link Emit#rollback(Savepoint)} — bundles the sink size with
 * {@link Emit}'s depth and the owed atom signature so a rollback puts all
 * three back in step (#7539).
 *
 * @since 0.1
 */
final class Savepoint {

    /** Sink size at the savepoint. */
    private final int sink;

    /** {@link Emit}'s depth at the savepoint. */
    private final int depth;

    /** {@link Emit}'s signature at the savepoint. */
    private final String signature;

    /** {@link Emit}'s sigline at the savepoint. */
    private final int sigline;

    /** {@link Emit}'s sigpos at the savepoint. */
    private final int sigpos;

    /** {@link Emit}'s sigdepth at the savepoint. */
    private final int sigdepth;

    /**
     * Ctor.
     *
     * @param sink Sink size at the savepoint
     * @param depth Open element depth at the savepoint
     * @param signature Owed atom signature at the savepoint
     * @param sigline Source line of the owed marker
     * @param sigpos Source column of the owed marker
     * @param sigdepth Depth of the object owing the marker
     */
    Savepoint(
        final int sink, final int depth, final String signature,
        final int sigline, final int sigpos, final int sigdepth
    ) {
        this.sink = sink;
        this.depth = depth;
        this.signature = signature;
        this.sigline = sigline;
        this.sigpos = sigpos;
        this.sigdepth = sigdepth;
    }

    /**
     * Sink size at the savepoint.
     *
     * @return Sink size
     */
    int sink() {
        return this.sink;
    }

    /**
     * Put {@link Emit}'s depth and owed atom signature back to what they
     * were at this savepoint.
     *
     * @param emit Emitter to restore
     */
    void restore(final Emit emit) {
        emit.depth(this.depth);
        emit.signature(this.signature, this.sigline, this.sigpos, this.sigdepth);
    }
}
