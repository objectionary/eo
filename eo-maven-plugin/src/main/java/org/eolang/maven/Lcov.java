/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
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
 *     A record that arrives twice still counts once, and a record that is
 *     not {@code program:line:pos} is left out, since the tests append to
 *     that file from more than one thread and a torn line must not cost
 *     the whole report. The {@code DA:} counter deliberately reuses the
 *     execution-count field of LCOV, and until the untouched objects are
 *     named too, {@code LH} equals {@code LF} and every consumer of this
 *     file shows a hundred per cent.
 * </p>
 * @since 0.74.0
 */
final class Lcov {

    /** The shape of a record {@code PhCoverage} appends. */
    private static final Pattern RECORD = Pattern.compile("([^:]+):(\\d+):(\\d+)");

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
            final Matcher parts = Lcov.RECORD.matcher(record);
            if (parts.matches()) {
                counts
                    .computeIfAbsent(parts.group(1), program -> new TreeMap<>())
                    .merge(Integer.valueOf(parts.group(2)), 1, Integer::sum);
            } else {
                Logger.warn(
                    this,
                    "The coverage record '%s' is skipped, while 'program:line:pos' is expected",
                    record
                );
            }
        }
        return counts;
    }
}
