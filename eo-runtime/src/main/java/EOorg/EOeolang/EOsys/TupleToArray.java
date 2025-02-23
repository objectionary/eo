/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys; // NOPMD

import java.util.function.Supplier;
import org.eolang.Data;
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
        final Phi retriever = this.tuple.take("at");
        final int length = new Dataized(this.tuple.take("length")).asNumber().intValue();
        final Phi[] arguments = new Phi[length];
        for (int iter = 0; iter < length; ++iter) {
            final Phi taken = retriever.copy();
            taken.put(0, new Data.ToPhi(iter));
            arguments[iter] = taken;
        }
        return arguments;
    }
}
