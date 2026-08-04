/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * An LCOV tracefile of EO object coverage.
 * <p>
 *     It merges the locations {@code PhCoverage} recorded while the tests
 *     ran against every location the transpiler instrumented. Both
 *     collections hold {@code program:line:pos} records, the shape
 *     {@code PhCoverage} appends, so a program turns into an {@code SF:}
 *     path and a line into a {@code DA:} counter of every hit recorded
 *     for the objects of that line. A record the run touched but the
 *     transpiler never instrumented is left out.
 * </p>
 * @since 0.58
 */
final class Lcov {

    /**
     * Every location the transpiler instrumented, as {@code program:line:pos}.
     */
    private final Collection<String> located;

    /**
     * Every location the run touched, as {@code program:line:pos}.
     */
    private final Collection<String> recorded;

    /**
     * Ctor.
     * @param instrumented Every location the transpiler instrumented
     * @param hits Every location the run touched
     */
    Lcov(final Collection<String> instrumented, final Collection<String> hits) {
        this.located = instrumented;
        this.recorded = hits;
    }

    @Override
    public String toString() {
        return this.counted().entrySet().stream()
            .map(
                program -> String.format(
                    "TN:%nSF:%s.eo%n%sLF:%d%nLH:%d%nend_of_record%n",
                    program.getKey().replace('.', '/'),
                    program.getValue().entrySet().stream()
                        .map(line -> String.format("DA:%d,%d%n", line.getKey(), line.getValue()))
                        .collect(Collectors.joining()),
                    program.getValue().size(),
                    program.getValue().values().stream().filter(hit -> hit > 0L).count()
                )
            )
            .collect(Collectors.joining());
    }

    /**
     * How many times each instrumented line of each program was touched.
     * @return Programs in alphabetical order, each with its lines in order
     */
    private Map<String, Map<Integer, Long>> counted() {
        final Map<String, Long> hits = this.recorded.stream()
            .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
        final Map<String, Map<Integer, Long>> counts = new TreeMap<>();
        for (final String record : this.located) {
            final String[] parts = record.split(":");
            counts
                .computeIfAbsent(parts[0], program -> new TreeMap<>())
                .merge(Integer.valueOf(parts[1]), hits.getOrDefault(record, 0L), Long::sum);
        }
        return counts;
    }
}
