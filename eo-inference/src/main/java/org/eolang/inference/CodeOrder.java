/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.Serializable;
import java.util.Comparator;
import java.util.PrimitiveIterator;

/**
 * The order of two strings by their code points, one by one.
 *
 * <p>{@link String#compareTo(String)} compares UTF-16 units, and a character
 * outside the basic plane, such as the {@code 🌵} of a name the parser made,
 * is two units that start below {@code U+E000}, so it lands before
 * {@code U+FF01}, where its code point lands after. The report is drawn the
 * way the Python one is, which compares code points, so every list it sorts
 * is sorted this way.</p>
 *
 * @since 0.76.0
 */
final class CodeOrder implements Comparator<String>, Serializable {

    /**
     * Serialization marker.
     */
    private static final long serialVersionUID = 0x9650L;

    @Override
    public int compare(final String first, final String second) {
        final PrimitiveIterator.OfInt left = first.codePoints().iterator();
        final PrimitiveIterator.OfInt right = second.codePoints().iterator();
        int found = 0;
        while (found == 0 && left.hasNext() && right.hasNext()) {
            found = Integer.compare(left.nextInt(), right.nextInt());
        }
        if (found == 0) {
            found = Boolean.compare(left.hasNext(), right.hasNext());
        }
        return found;
    }
}
