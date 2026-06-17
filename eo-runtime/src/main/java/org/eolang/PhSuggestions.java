/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Suggestions for EO objects.
 * @since 1.0
 */
final class PhSuggestions {

    /**
     * Default number of suggestions.
     */
    private static final int LIMIT = 5;

    /**
     * No candidates.
     */
    private static final Collection<String> EMPTY = List.of();

    /**
     * Candidates.
     */
    private final Collection<String> candidates;

    /**
     * Read candidates from classpath.
     */
    private final boolean classpath;

    /**
     * Ctor.
     */
    PhSuggestions() {
        this(PhSuggestions.EMPTY, true);
    }

    /**
     * Ctor.
     * @param candidates Candidates
     */
    PhSuggestions(final Collection<String> candidates) {
        this(candidates, false);
    }

    /**
     * Ctor.
     * @param candidates Candidates
     * @param classpath Read candidates from classpath
     */
    private PhSuggestions(final Collection<String> candidates, final boolean classpath) {
        this.candidates = candidates;
        this.classpath = classpath;
    }

    /**
     * Make a message with suggestions.
     * @param name Requested EO object name
     * @return Message suffix
     */
    String message(final String name) {
        return this.message(name, PhSuggestions.LIMIT);
    }

    /**
     * Make a message with suggestions.
     * @param name Requested EO object name
     * @param limit Max suggestions
     * @return Message suffix
     */
    String message(final String name, final int limit) {
        final String suffix;
        final List<String> found = this.suggestions(name, limit);
        if (found.isEmpty()) {
            suffix = "";
        } else {
            suffix = String.format(
                "%n%nDid you mean?%n%s",
                found.stream()
                    .map(item -> String.format("  - %s", item))
                    .collect(Collectors.joining(System.lineSeparator()))
            );
        }
        return suffix;
    }

    /**
     * Suggest objects.
     * @param name Requested EO object name
     * @param limit Max suggestions
     * @return Suggestions
     */
    List<String> suggestions(final String name, final int limit) {
        final String normalized = PhSuggestionName.normalize(name);
        return this.candidates().stream()
            .map(PhSuggestionName::normalize)
            .filter(Predicate.not(String::isEmpty))
            .distinct()
            .filter(candidate -> !candidate.equals(normalized))
            .map(candidate -> PhRanked.ranked(normalized, candidate))
            .filter(PhRanked::sufficient)
            .sorted()
            .limit(limit)
            .map(ranked -> PhSuggestionName.display(name, ranked.name()))
            .collect(Collectors.toList());
    }

    /**
     * Object names by class resource path.
     * @param resource Resource path
     * @return EO object names
     */
    static Collection<String> names(final String resource) {
        return PhObjectNames.names(resource);
    }

    /**
     * Available candidates.
     * @return Candidates
     */
    private Collection<String> candidates() {
        final Collection<String> found;
        if (this.classpath) {
            found = PhClasspathObjects.discover();
        } else {
            found = this.candidates;
        }
        return found;
    }
}
