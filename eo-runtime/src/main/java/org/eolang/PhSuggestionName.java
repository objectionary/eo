/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Locale;

/**
 * EO suggestion display name.
 * @since 1.0
 */
final class PhSuggestionName {

    /**
     * Java root package.
     */
    private static final String ROOT = "org.eolang.";

    /**
     * EO global object.
     */
    private static final String GLOBAL = "Φ";

    /**
     * Ctor.
     */
    private PhSuggestionName() {
    }

    /**
     * Normalize EO object name.
     * @param name Name
     * @return Normalized name
     */
    static String normalize(final String name) {
        String normalized = name;
        if (normalized.startsWith(String.format("%s.", PhSuggestionName.GLOBAL))) {
            normalized = normalized.substring(PhSuggestionName.GLOBAL.length() + 1);
        }
        if (normalized.startsWith(PhSuggestionName.ROOT)) {
            normalized = normalized.substring(PhSuggestionName.ROOT.length());
        }
        return normalized.toLowerCase(Locale.ENGLISH);
    }

    /**
     * Display suggestion in the same namespace style.
     * @param origin Origin
     * @param suggestion Suggestion
     * @return Displayed suggestion
     */
    static String display(final String origin, final String suggestion) {
        final String displayed;
        if (
            origin.startsWith(
                String.format("%s.%s", PhSuggestionName.GLOBAL, PhSuggestionName.ROOT)
            ) || origin.startsWith(PhSuggestionName.ROOT)
        ) {
            displayed = String.format("%s%s", PhSuggestionName.ROOT, suggestion);
        } else {
            displayed = suggestion;
        }
        return displayed;
    }
}
