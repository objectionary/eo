/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Which of ten bands a place falls in, by what it answers over all its copies.
 *
 * <p>A place is green when it answers one name and nothing else in any copy,
 * lime when that name comes with a gap in other copies, teal when it answers
 * exactly two names and nothing else, and blue when it answers two or more
 * names any other way. A gap is an error ({@code T}) or an unknown. With no
 * name at all, a place is brown when it answers only errors, and otherwise
 * takes the colour of the kind of unknown it answers: pink for a read
 * nothing ever answers, purple for a void only Java fills, slate for a void
 * of an object nobody calls, and amber for a void nobody fills. A place that
 * answers several kinds takes the first of them in that order. A forbidden
 * construction in any copy makes it red whatever else it answers; today the
 * only one is a {@code ^} that only Java fills.</p>
 *
 * <p>The labels are the ones the place answers, with the formations of one
 * shape folded into one name first, so two copies of one lambda count as one
 * name and not as two. The band is worked out here and nowhere else, so that
 * the colour of a mark and the tally above it cannot disagree.</p>
 *
 * @since 0.71.0
 */
final class Band {

    /**
     * The bands, in the order a tally lists them.
     */
    static final List<String> ORDER = Arrays.asList(
        "named", "gaps", "pair", "mixed", "loop", "atom", "study", "empty", "error", "forbidden"
    );

    /**
     * The kinds of an unknown, the one a place is coloured by first.
     */
    private static final List<String> KINDS = Arrays.asList(
        "?unknown-loop", "?unknown-java", "?unknown-study", "?unknown-empty"
    );

    /**
     * The band of a place whose only gaps are unknowns, by the kind it is
     * coloured by.
     */
    private static final Map<String, String> UNKNOWNS = Map.of(
        "?unknown-loop", "loop",
        "?unknown-java", "atom",
        "?unknown-study", "study",
        "?unknown-empty", "empty"
    );

    /**
     * The labels the place answers, each shape folded into one name.
     */
    private final Collection<String> labels;

    /**
     * Ctor.
     *
     * @param said The labels the place answers, each shape folded into one name
     */
    Band(final Collection<String> said) {
        this.labels = said;
    }

    /**
     * The name of the band.
     *
     * @return The name, which a page puts on a mark and a tally counts under
     */
    String name() {
        final Set<String> all = new HashSet<>(this.labels);
        final Set<String> names = all.stream()
            .filter(label -> !label.startsWith("?") && !label.startsWith("!") && !"T".equals(label))
            .collect(Collectors.toSet());
        final String found;
        if (all.stream().anyMatch(label -> label.startsWith("!"))) {
            found = "forbidden";
        } else if (all.equals(names)) {
            found = Band.named(names.size());
        } else if (names.isEmpty()) {
            found = this.unnamed();
        } else if (names.size() == 1) {
            found = "gaps";
        } else {
            found = "mixed";
        }
        return found;
    }

    private static String named(final int count) {
        final String found;
        if (count == 1) {
            found = "named";
        } else if (count == 2) {
            found = "pair";
        } else {
            found = "mixed";
        }
        return found;
    }

    private String unnamed() {
        final Set<String> unknown = this.labels.stream()
            .filter(label -> label.startsWith("?"))
            .collect(Collectors.toSet());
        final String found;
        if (unknown.isEmpty()) {
            found = "error";
        } else {
            found = Band.UNKNOWNS.get(
                Band.KINDS.stream().filter(unknown::contains).findFirst().orElseThrow(
                    () -> new IllegalStateException(
                        String.format(
                            "The place answers %s, and no unknown of a kind the report colours",
                            unknown
                        )
                    )
                )
            );
        }
        return found;
    }
}
