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
 * touch, one {@code program:line:pos} line is appended, at most once per
 * JVM; without the property every hit is a silent no-op. The property
 * is re-read on every touch (not cached at class load), since this
 * class is now instantiated around every located object in every EO
 * program: the very first one touched anywhere in the JVM would
 * otherwise freeze a stale answer for the rest of the run. Thread-safe.</p>
 *
 * <p>The {@code coverageTracking} parameter of eo-maven-plugin's
 * {@code transpile} goal ({@code MjTranspile}) controls whether the
 * transpiler wraps located objects into this decorator in the first
 * place, and its {@code lcov} goal turns the file this class appends to
 * into an LCOV tracefile. What is still missing is the untouched objects
 * in that tracefile and a threshold that fails the build on poor
 * coverage; see the javadoc of {@code MjLcov}.</p>
 *
 * @since 0.58
 */
public final class PhCoverage implements Phi {

    /** Locations already written in this JVM. */
    private static final Set<String> SEEN = ConcurrentHashMap.newKeySet();

    /** The origin. */
    private final Phi origin;

    /**
     * The name of the EO program that holds the object, dot separated.
     * Together with {@link #line} and {@link #pos} it identifies the object.
     */
    private final String program;

    /** Source line. */
    private final int line;

    /** Source column. */
    private final int pos;

    /**
     * Ctor.
     * @param phi The origin
     * @param name The name of the EO program that holds the object
     * @param lne Source line
     * @param position Source column
     */
    public PhCoverage(final Phi phi, final String name, final int lne, final int position) {
        this.origin = phi;
        this.program = name;
        this.line = lne;
        this.pos = position;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }

    @Override
    public Phi copy() {
        return new PhCoverage(this.origin.copy(), this.program, this.line, this.pos);
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
        final String property = PhCoverage.property();
        if (property != null) {
            final String record = String.format("%s:%d:%d%n", this.program, this.line, this.pos);
            if (PhCoverage.SEEN.add(record)) {
                try {
                    Files.write(
                        Paths.get(property),
                        record.getBytes(StandardCharsets.UTF_8),
                        StandardOpenOption.CREATE,
                        StandardOpenOption.APPEND
                    );
                } catch (final IOException ex) {
                    throw new UncheckedIOException(
                        String.format("Failed to append a coverage hit to '%s'", property),
                        ex
                    );
                } catch (final InvalidPathException ex) {
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

    /**
     * Value of the {@code eo.coverageFile} property.
     * @return The value, or NULL when recording is disabled
     */
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
