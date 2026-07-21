/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
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
 * JVM; without the property every hit is a silent no-op. The property
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
 */
@SuppressWarnings("PMD.TooManyMethods")
public final class PhCoverage implements Phi {

    /** Locations already written in this JVM. */
    private static final Set<String> SEEN = ConcurrentHashMap.newKeySet();

    /** The origin. */
    private final Phi origin;

    /** The fully qualified location of the object. */
    private final String loc;

    /** Source line. */
    private final int line;

    /** Source column. */
    private final int pos;

    /**
     * Ctor.
     * @param phi The origin
     * @param location The fully qualified location of the object
     * @param lne Source line
     * @param position Source column
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    public PhCoverage(final Phi phi, final String location, final int lne, final int position) {
        this.origin = phi;
        this.loc = location;
        this.line = lne;
        this.pos = position;
    }

    @Override
    public Phi copy() {
        return new PhCoverage(this.origin.copy(), this.loc, this.line, this.pos);
    }

    @Override
    public boolean hasRho() {
        return this.origin.hasRho();
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

    /** Record one hit of this location, at most once per JVM. */
    private void hit() {
        final Path target = PhCoverage.target();
        if (target != null) {
            final String record = String.format("%s:%d:%d%n", this.loc, this.line, this.pos);
            if (PhCoverage.SEEN.add(record)) {
                try {
                    Files.write(
                        target,
                        record.getBytes(StandardCharsets.UTF_8),
                        StandardOpenOption.CREATE,
                        StandardOpenOption.APPEND
                    );
                } catch (final IOException ex) {
                    throw new UncheckedIOException(
                        String.format("Failed to append a coverage hit to '%s'", target),
                        ex
                    );
                }
            }
        }
    }

    /**
     * Target file from the {@code eo.coverageFile} property.
     * @return The path, or NULL when recording is disabled
     */
    private static Path target() {
        final String path = System.getProperty("eo.coverageFile");
        final Path result;
        if (path == null || path.isEmpty()) {
            result = null;
        } else {
            result = Paths.get(path);
        }
        return result;
    }
}
