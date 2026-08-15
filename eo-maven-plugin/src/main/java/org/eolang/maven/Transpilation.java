/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;
import org.eolang.parser.TrFull;

/**
 * The XSL train that turns an XMIR into Java, and the cache key of its output.
 *
 * <p>The train is the same for every XMIR of a build, so it is compiled once
 * per thread and reused. The key tells a cached result of an earlier build
 * apart from the one this train would produce now.</p>
 *
 * @since 0.62.0
 */
final class Transpilation {

    /**
     * The XSL steps of the transpile train, in order, ending with
     * {@code to-java.xsl}. Kept as a single list so both the train in
     * {@link #compiled(boolean, boolean, String)} and the cache-key fingerprint in
     * {@link #version()} are derived from the same source.
     */
    static final String[] XSLS = {
        "/org/eolang/parser/parse/set-locators.xsl",
        "/org/eolang/maven/transpile/set-original-names.xsl",
        "/org/eolang/maven/transpile/classes.xsl",
        "/org/eolang/maven/transpile/tests.xsl",
        "/org/eolang/maven/transpile/anonymous-to-nested.xsl",
        "/org/eolang/maven/transpile/package.xsl",
        "/org/eolang/maven/transpile/attrs.xsl",
        "/org/eolang/maven/transpile/data.xsl",
        "/org/eolang/maven/transpile/to-java.xsl",
    };

    /**
     * Classpath resources {@code xsl:import}-ed by one or more of
     * {@link #XSLS}, so their content must also be folded into the
     * cache-key fingerprint in {@link #version()} — editing one of these
     * shared libraries changes the actual transpile output just as much
     * as editing a top-level stylesheet does, but leaves {@link #XSLS}
     * itself unchanged (see #6032). Not part of {@link #XSLS} itself
     * because that array is also used verbatim to build the actual XSL
     * train in {@link #compiled(boolean, boolean, String)}, where its last
     * element is special-cased as {@code to-java.xsl}.
     */
    static final String[] IMPORTS = {
        "/org/eolang/parser/_funcs.xsl",
        "/org/eolang/parser/_specials.xsl",
    };

    /**
     * Parsing trains with XSLs, one per thread, keyed by whether source
     * locations are tracked.
     *
     * <p>A single shared instance is deliberately avoided: {@link TrClasspath}
     * and {@link StClasspath} compile their XSL stylesheets lazily on
     * first use, and that lazy compilation is not thread-safe. Sharing one
     * instance across the {@link Threaded} worker pool races on the same
     * underlying compilation cache and can produce truncated or garbled
     * Java output. Keeping one instance per thread still lets each worker
     * thread reuse its own compiled stylesheets across the sources it
     * processes.</p>
     */
    private static final ThreadLocal<Map<String, Train<Shift>>> TRAINS =
        ThreadLocal.withInitial(HashMap::new);

    /**
     * Plugin version.
     */
    private final String version;

    /**
     * Which optional diagnostic artifacts to emit while transpiling.
     */
    private final Tracking tracking;

    /**
     * What the transpiler writes into the generated Java.
     */
    private final PhiSettings settings;

    /**
     * Where the transpiler writes.
     */
    private final Outputs outputs;

    /**
     * Ctor.
     * @param ver Plugin version string
     * @param diagnostics Which diagnostic artifacts to emit while transpiling
     * @param settings What the transpiler writes into the generated Java
     * @param outputs Where the transpiler writes
     */
    Transpilation(
        final String ver,
        final Tracking diagnostics,
        final PhiSettings settings,
        final Outputs outputs
    ) {
        this.version = ver;
        this.tracking = diagnostics;
        this.settings = settings;
        this.outputs = outputs;
    }

    /**
     * Cache-key version segment: the plugin version combined with a
     * fingerprint of the bundled transpile XSLs and the libraries they
     * {@code xsl:import}, plus the {@code trackLocations}/
     * {@code coverageTracking} flags. Folding the XSL content in means
     * that a change in the transformation logic invalidates the global
     * transpile cache even when the plugin version is unchanged (a
     * constant {@code -SNAPSHOT} during development), see #5578; folding
     * the imported libraries in too closes the gap where editing one of
     * them changed the actual output without changing anything in
     * {@link #XSLS} itself, see #6032. Folding the two flags and the name
     * of the base class in means changing any of them also invalidates the
     * cache, since all of them change what {@code to-java.xsl} emits (see
     * #6031 and #5955).
     * @return The version segment for {@link CachePath}
     */
    String version() {
        return String.format(
            "%s-%s-%b-%b-%s",
            this.version,
            new Fingerprint(
                Stream.concat(
                    Arrays.stream(Transpilation.XSLS), Arrays.stream(Transpilation.IMPORTS)
                ).toArray(String[]::new)
            ).get(),
            this.tracking.locations(), this.settings.coverage(), this.settings.superclass()
        );
    }

    /**
     * Build XSL transformation function for a source file.
     * If transformation steps are tracked - creates a new {@link Xsline}
     * for every XMIR in purpose of thread safety.
     * @param name Name of the object the source XMIR holds
     * @return XSL transformation function
     */
    Function<XML, XML> forSource(final String name) {
        final Train<Shift> measured = this.measured(this.train());
        final Function<XML, XML> func;
        if (this.tracking.steps()) {
            final Path dir = new Place(name).make(
                this.outputs.target().resolve(Transpiling.PRE), ""
            );
            func = xml -> new Xsline(new TrSpy(measured, dir)).pass(xml);
        } else {
            func = new Xsline(measured)::pass;
        }
        return func;
    }

    /**
     * Wrap a train with XSL execution time measurements.
     * @param base The train to wrap
     * @return Measured train
     */
    private Train<Shift> measured(final Train<Shift> base) {
        final Path parent = this.outputs.measures().toAbsolutePath().getParent();
        if (parent.toFile().mkdirs()) {
            Logger.debug(this, "Directory created for %[file]s", this.outputs.measures());
        }
        if (!Files.exists(parent)) {
            throw new IllegalArgumentException(
                String.format(
                    "For some reason, the directory %s is absent, can't write measures to %s",
                    parent,
                    this.outputs.measures()
                )
            );
        }
        if (Files.isDirectory(this.outputs.measures())) {
            throw new IllegalArgumentException(
                String.format(
                    "This is not a file but a directory, can't write to it: %s",
                    this.outputs.measures()
                )
            );
        }
        return new TrLambda(
            base,
            shift -> new StMeasured(shift, this.outputs.measures())
        );
    }

    /**
     * The train for this thread, built once per location-tracking value.
     * @return The train of XSL shifts
     */
    private Train<Shift> train() {
        final boolean track = this.tracking.locations();
        final boolean instrument = this.settings.coverage();
        final String base = this.settings.superclass();
        return Transpilation.TRAINS.get().computeIfAbsent(
            String.format("%b|%b|%s", track, instrument, base),
            ignored -> Transpilation.compiled(track, instrument, base)
        );
    }

    /**
     * Build the train of XSL shifts.
     * @param track Whether generated objects carry their source location
     * @param instrument Whether located objects are wrapped into {@code PhCoverage}
     * @param base The class that a generated class extends instead of {@code PhDefault}
     * @return The train of XSL shifts
     */
    private static Train<Shift> compiled(
        final boolean track, final boolean instrument, final String base
    ) {
        return new TrFull(
            new TrJoined<>(
                new TrClasspath<>(
                    Arrays.copyOf(Transpilation.XSLS, Transpilation.XSLS.length - 1)
                ).back(),
                new TrDefault<>(
                    new StClasspath(
                        Transpilation.XSLS[Transpilation.XSLS.length - 1],
                        String.format("disclaimer %s", new Disclaimer()),
                        String.format("trackLocations %b", track),
                        String.format("coverage %b", instrument),
                        String.format("phiDefaultClass %s", base)
                    )
                )
            )
        );
    }
}
