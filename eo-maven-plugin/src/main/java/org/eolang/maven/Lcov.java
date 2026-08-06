/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
import org.cactoos.set.SetOf;

/**
 * An LCOV tracefile of the EO objects a run touched.
 * <p>
 *     It turns the {@code program:line:pos} records {@code PhCoverage}
 *     appends into LCOV, which Codecov, Coveralls and {@code genhtml}
 *     read as is: a program becomes an {@code SF:} path under the
 *     directory of {@code .eo} sources it is given, and a line becomes a
 *     {@code DA:} counter of how many objects of that line were touched.
 *     A record that arrives twice still counts once.
 * </p>
 * @since 0.58
 */
final class Lcov {

    /** The directory that holds the {@code .eo} sources. */
    private final Path sources;

    /** Every location the run touched, as {@code program:line:pos}. */
    private final Collection<String> hits;

    /**
     * Ctor.
     * @param sources The directory that holds the {@code .eo} sources
     * @param hits Every location the run touched
     */
    Lcov(final Path sources, final Iterable<String> hits) {
        this.sources = sources;
        this.hits = new SetOf<>(hits);
    }

    @Override
    public String toString() {
        return this.counted().entrySet().stream().map(
            program -> String.format(
                "TN:%nSF:%s%n%sLF:%d%nLH:%d%nend_of_record%n",
                this.sources.resolve(
                    String.format("%s.eo", program.getKey().replace('.', '/'))
                ),
                program.getValue().entrySet().stream()
                    .map(line -> String.format("DA:%d,%d%n", line.getKey(), line.getValue()))
                    .collect(Collectors.joining()),
                program.getValue().size(),
                program.getValue().size()
            )
        ).collect(Collectors.joining());
    }

    /**
     * How many objects of each line of each program the run touched.
     * @return Programs in alphabetical order, each with its lines in order
     */
    private Map<String, Map<Integer, Integer>> counted() {
        final Map<String, Map<Integer, Integer>> counts = new TreeMap<>();
        for (final String record : this.hits) {
            final String[] parts = record.split(":");
            if (parts.length != 3) {
                throw new IllegalArgumentException(
                    String.format(
                        "The coverage record is '%s', while 'program:line:pos' is expected", record
                    )
                );
            }
            counts
                .computeIfAbsent(parts[0], program -> new TreeMap<>())
                .merge(Integer.valueOf(parts[1]), 1, Integer::sum);
        }
        return counts;
    }
}
