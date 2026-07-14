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
 * <p>The transpiler emits this decorator around every located object
 * when coverage instrumentation is requested. Recording is enabled by
 * the {@code eo.coverage.file} system property: the first time an
 * instrumented object is touched, one {@code loc:line:pos} line is
 * appended to that file, at most once per JVM. Without the property
 * every hit is a silent no-op. This class is thread-safe.</p>
 *
 * @since 0.58
 */
@SuppressWarnings("PMD.TooManyMethods")
public final class PhCoverage implements Phi {

    /** Locations already written in this JVM. */
    private static final Set<String> SEEN = ConcurrentHashMap.newKeySet();

    /** The file to append hits to, or NULL when recording is disabled. */
    private static final Path TARGET = PhCoverage.target();

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

    /**
     * Record one hit of this location, at most once per JVM.
     */
    private void hit() {
        if (PhCoverage.TARGET != null) {
            final String record = String.format("%s:%d:%d%n", this.loc, this.line, this.pos);
            if (PhCoverage.SEEN.add(record)) {
                try {
                    Files.write(
                        PhCoverage.TARGET,
                        record.getBytes(StandardCharsets.UTF_8),
                        StandardOpenOption.CREATE,
                        StandardOpenOption.APPEND
                    );
                } catch (final IOException ex) {
                    throw new UncheckedIOException(
                        String.format("Failed to append a coverage hit to '%s'", PhCoverage.TARGET),
                        ex
                    );
                }
            }
        }
    }

    /**
     * Resolve the target file from the system property.
     * @return The path, or NULL when recording is disabled
     */
    private static Path target() {
        final String path = System.getProperty("eo.coverage.file");
        final Path result;
        if (path == null || path.isEmpty()) {
            result = null;
        } else {
            result = Paths.get(path);
        }
        return result;
    }
}
