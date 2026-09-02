/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * An object that records a coverage hit of its source location when
 * it is touched, and delegates everything to the origin.
 *
 * <p>The transpiler emits it around every located object. Recording is
 * enabled by the {@code eo.coverageFile} system property: on the first
 * touch, one {@code loc:line:pos} line is appended, at most once per
 * wrapper per configured destination; without the property every hit is
 * a silent no-op. The property
 * is re-read on every touch (not cached at class load), since this
 * class is now instantiated around every located object in every EO
 * program: the very first one touched anywhere in the JVM would
 * otherwise freeze a stale answer for the rest of the run. Thread-safe.</p>
 *
 * <p>The {@code coverageFile} parameter of eo-maven-plugin's
 * {@code transpile} goal ({@code MjTranspile}) controls whether the
 * transpiler wraps located objects into this decorator in the first
 * place; see its javadoc for the remaining gaps (the LCOV report and
 * threading this same file to the process that runs the compiled
 * program).</p>
 *
 * @since 0.58
 * @todo #6508:30min Let one set of hits span the whole program. Every wrapper
 *  the transpiler builds starts with an empty set of its own, so a location
 *  touched through many instances of the same object is appended to the file
 *  once per instance, the file fills up with duplicates, and every wrapper
 *  pays for a set it shares with nobody. The generated code should hand the
 *  same set to every wrapper it builds.
 */
public final class PhCoverage implements Phi {

    /** The origin. */
    private final Phi origin;

    /** Locations written by this object and its copies, per destination. */
    private final Set<String> hits;

    /** The location to write, as {@code loc:line:pos}. */
    private final String record;

    /**
     * Ctor.
     * @param phi The origin
     * @param mark The location to write, as {@code loc:line:pos}
     */
    public PhCoverage(final Phi phi, final String mark) {
        this(phi, ConcurrentHashMap.newKeySet(), mark);
    }

    /**
     * Ctor.
     * @param phi The origin
     * @param seen Locations written already, per destination
     * @param mark The location to write, as {@code loc:line:pos}
     */
    public PhCoverage(final Phi phi, final Set<String> seen, final String mark) {
        this.origin = phi;
        this.hits = seen;
        this.record = mark;
    }

    @Override
    public Phi copy() {
        return new PhCoverage(this.origin.copy(), this.hits, this.record);
    }

    @Override
    public boolean needsRho() {
        return this.origin.needsRho();
    }

    @Override
    public Phi take(final String name) {
        this.hit();
        return this.origin.take(name);
    }

    @Override
    public void put(final int position, final Phi object) {
        this.origin.put(position, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.origin.put(name, object);
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

    @Override
    public Phi normalized() {
        this.hit();
        return this.origin.normalized();
    }

    @Override
    public byte[] delta() {
        this.hit();
        return this.origin.delta();
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    private void hit() {
        final String property = PhCoverage.property();
        if (property != null) {
            final String seen = property + '\0' + this.record;
            if (this.hits.add(seen)) {
                try {
                    Files.write(
                        Paths.get(property),
                        String.format("%s%n", this.record).getBytes(StandardCharsets.UTF_8),
                        StandardOpenOption.CREATE,
                        StandardOpenOption.APPEND
                    );
                } catch (final IOException ex) {
                    this.hits.remove(seen);
                    throw new UncheckedIOException(
                        String.format("Failed to append a coverage hit to '%s'", property),
                        ex
                    );
                } catch (final InvalidPathException ex) {
                    this.hits.remove(seen);
                    throw new IllegalArgumentException(
                        String.format(
                            "Invalid path '%s' in the 'eo.coverageFile' system property",
                            property
                        ),
                        ex
                    );
                }
            }
        }
    }

    private static String property() {
        final String path = System.getProperty("eo.coverageFile");
        final String result;
        if (path == null || path.isEmpty()) {
            result = null;
        } else {
            result = path;
        }
        return result;
    }
}
