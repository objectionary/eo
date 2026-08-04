/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

/**
 * An LCOV tracefile of EO object coverage.
 * <p>
 *     It merges the locations {@code PhCoverage} recorded while the tests
 *     ran against every location the transpiler instrumented, and prints
 *     the result in the LCOV format, the one Codecov, Coveralls and
 *     {@code genhtml} read. Both collections hold {@code program:line:pos}
 *     records, the same shape {@code PhCoverage} appends, so a program
 *     turns into an {@code SF:} path and a line into a {@code DA:} counter
 *     carrying the number of instrumented positions of that line the run
 *     touched. A record the run touched but the transpiler never
 *     instrumented belongs to another build and is left out.
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
            .map(program -> this.tracefile(program.getKey(), program.getValue()))
            .collect(Collectors.joining());
    }

    /**
     * How many times each instrumented line of each program was touched.
     * @return Programs in alphabetical order, each with its lines in order
     */
    private Map<String, Map<Integer, Long>> counted() {
        final Set<String> hits = new HashSet<>(this.recorded);
        final Map<String, Map<Integer, Long>> counts = new TreeMap<>();
        for (final String record : this.located) {
            final String[] parts = record.split(":");
            counts
                .computeIfAbsent(parts[0], program -> new TreeMap<>())
                .merge(Integer.valueOf(parts[1]), hits.contains(record) ? 1L : 0L, Long::sum);
        }
        return counts;
    }

    /**
     * One tracefile record.
     * @param program The name of the EO program, dot separated
     * @param lines How many times each instrumented line of it was touched
     * @return The record, ending with a line break
     */
    private String tracefile(final String program, final Map<Integer, Long> lines) {
        return String.format(
            "TN:%nSF:%s.eo%n%sLF:%d%nLH:%d%nend_of_record%n",
            program.replace('.', '/'),
            lines.entrySet().stream()
                .map(line -> String.format("DA:%d,%d%n", line.getKey(), line.getValue()))
                .collect(Collectors.joining()),
            lines.size(),
            lines.values().stream().filter(hit -> hit > 0L).count()
        );
    }
}
