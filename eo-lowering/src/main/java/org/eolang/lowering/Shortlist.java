/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collection;
import org.cactoos.Text;
import org.cactoos.iterable.HeadOf;

/**
 * A list of names, cut short so that it fits into one line of the log.
 *
 * <p>When the list is short, every name is shown, with a comma between
 * two names. When the list is longer than the limit, only the first names
 * are shown, and the text ends with how many names are left out, like
 * {@code number.exp, number.power, and 67 more}. This way, a reader of the
 * log sees a few examples, and the line does not grow to hundreds of
 * names.</p>
 *
 * @since 0.64.0
 */
final class Shortlist implements Text {

    /**
     * All the names, in the order they must be shown.
     */
    private final Collection<String> names;

    /**
     * How many names may be shown at most.
     */
    private final int limit;

    /**
     * Ctor.
     *
     * @param all All the names, in the order they must be shown
     * @param max How many names may be shown at most
     */
    Shortlist(final Collection<String> all, final int max) {
        this.names = all;
        this.limit = max;
    }

    @Override
    public String asString() {
        final String shown = String.join(", ", new HeadOf<>(this.limit, this.names));
        final String text;
        if (this.names.size() > this.limit) {
            text = String.format("%s, and %d more", shown, this.names.size() - this.limit);
        } else {
            text = shown;
        }
        return text;
    }
}
