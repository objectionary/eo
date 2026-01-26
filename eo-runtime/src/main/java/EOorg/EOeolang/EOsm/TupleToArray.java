/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm; // NOPMD

import java.util.function.Supplier;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * Convert {@link EOorg.EOeolang.EOtuple} of arguments to Java array.
 * @since 0.40.0
 */
final class TupleToArray implements Supplier<Phi[]> {
    /**
     * Tuple of arguments.
     */
    private final Phi tuple;

    /**
     * Ctor.
     * @param tup Tuple of arguments.
     */
    TupleToArray(final Phi tup) {
        this.tuple = tup;
    }

    @Override
    public Phi[] get() {
        final int length = new Dataized(this.tuple.take("length")).asNumber().intValue();
        final Phi[] arguments = new Phi[length];
        Phi tup = this.tuple;
        for (int idx = length - 1; idx >= 0; --idx) {
            arguments[idx] = tup.take("head");
            tup = tup.take("tail");
        }
        return arguments;
    }
}
