/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Arrays;
import java.util.Collection;
import java.util.Locale;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Text helpers for suggestions.
 * @since 1.0
 */
final class PhText {

    /**
     * Separators to remove for compact names.
     */
    private static final Pattern COMPACT = Pattern.compile("[_\\s./-]+");

    /**
     * Separators in EO tokens.
     */
    private static final Pattern TOKENS = Pattern.compile("(?=[A-Z])|[^A-Za-z]+");

    /**
     * Ctor.
     */
    private PhText() {
    }

    /**
     * Tokens of EO object name.
     * @param name EO object name
     * @return Tokens
     */
    static Collection<String> tokens(final String name) {
        return Arrays.stream(PhText.TOKENS.split(name))
            .map(part -> part.toLowerCase(Locale.ENGLISH))
            .filter(Predicate.not(String::isEmpty))
            .collect(Collectors.toList());
    }

    /**
     * Compact EO object name.
     * @param name EO object name
     * @return Compact name
     */
    static String compact(final String name) {
        return PhText.COMPACT.matcher(
            name.toLowerCase(Locale.ENGLISH)
        ).replaceAll("");
    }

    /**
     * Longest common prefix length.
     * @param left Left text
     * @param right Right text
     * @return Prefix length
     */
    static int commonPrefix(final String left, final String right) {
        final int max = Math.min(left.length(), right.length());
        int pos = 0;
        while (pos < max && left.charAt(pos) == right.charAt(pos)) {
            ++pos;
        }
        return pos;
    }
}
