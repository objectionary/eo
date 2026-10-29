/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.Serializable;
import java.util.Comparator;

/**
 * The order of two strings by their code points, one by one.
 *
 * <p>{@link String#compareTo(String)} compares UTF-16 units, and a character
 * outside the basic plane, such as the {@code 🌵} of a name the parser made,
 * is two units that start below {@code U+E000}, so it lands before
 * {@code U+FF01}, where its code point lands after. The engine writes its
 * answers and the report draws its pages the way the Python ones do, which
 * compare code points, so every list either of them sorts is sorted this
 * way.</p>
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
        int pos = 0;
        int found = 0;
        while (found == 0 && pos < first.length() && pos < second.length()) {
            final int point = first.codePointAt(pos);
            found = Integer.compare(point, second.codePointAt(pos));
            pos += Character.charCount(point);
        }
        if (found == 0) {
            found = Integer.compare(first.length(), second.length());
        }
        return found;
    }
}
