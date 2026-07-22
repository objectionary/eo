/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.manifests.Manifests;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.stream.Collectors;
import org.cactoos.list.ListOf;
import org.eolang.lints.Defect;
import org.eolang.lints.Severity;
import org.eolang.lints.Source;
import org.eolang.parser.OnDefault;
import org.eolang.parser.OnDetailed;
import org.eolang.wpa.Program;
import org.w3c.dom.Node;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Runs all lints and checks errors and warnings.
 * <p>
 *     This class goes through all XMIR files generated in the previous steps (see {@link MjParse}
 *     or {@link MjPull} goals) and runs all available lints on them.
 *     If any errors or warnings are found, they are logged to the console,
 *     and depending on the configuration, the build may fail.
 *     The linting results are also embedded back into the XMIR files for future reference.
 *     Lints might use caching to speed up the process on subsequent runs.
 *     Cached files are stored in the {@link #CACHE} directory.
 *     The results of linting are saved in the {@link #DIR} directory.
 * </p>
 * <p>
 *     Note: this class is intentionally named {@code Linting} rather than {@code Lint} to avoid
 *     a conflict with Maven's Plexus configurator. When a class named {@code Lint} exists in the
 *     plugin package, Plexus tries to instantiate it (via no-arg constructor) as the element type
 *     for any {@code lint} XML child element it encounters in plugin configuration.
 *     Naming the class {@code Linting} avoids this collision.
 * </p>
 * @since 0.31.0
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
final class Linting implements Step {

    /**
     * The directory where to lint to.
     */
    static final String DIR = "3-lint";

    /**
     * Subdirectory for linted cache.
     */
    static final String CACHE = "linted";

    /**
     * Scoped foreign tojos.
     */
    private final TjsForeign tojos;

    /**
     * Compile-scope tojos (for WPA).
     */
    private final TjsForeign compile;

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Path targetDir;

    /**
     * Cache base directory.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Path cacheDir;

    /**
     * Whether caching is enabled.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final boolean cacheEnabled;

    /**
     * Plugin version.
     */
    private final String version;

    /**
     * Source lints to skip.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Collection<String> skipSourceLints;

    /**
     * Program (WPA) lints to skip.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Collection<String> skipProgramLints;

    /**
     * Whether to skip experimental lints.
     * @checkstyle MemberNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.LongVariable")
    private final boolean skipExperimentalLints;

    /**
     * Whether to fail on warnings.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final boolean failOnWarning;

    /**
     * Whether to lint all sources as a package (WPA).
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final boolean lintAsPackage;

    /**
     * EO sources directory (for WPA cache key).
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Path sourcesDir;

    /**
     * Whether to skip linting entirely.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final boolean skipLinting;

    /**
     * Shared cache guard, reused across all files so that concurrent writes to
     * the same cache tail path are actually serialized (#5720).
     */
    private final ConcurrentCache guard;

    /**
     * Constructor.
     * @param srcs Scoped tojos
     * @param compiled Compile tojos
     * @param target Target directory
     * @param cache Base cache directory
     * @param enabled Whether caching is enabled
     * @param ver Plugin version
     * @param sourcelints Source lints to skip
     * @param programlints Program lints to skip
     * @param experimental Whether to skip experimental lints
     * @param warning Whether to fail on warnings
     * @param pkg Whether to lint all sources as a package
     * @param sources EO sources directory
     * @param skip Whether to skip linting entirely
     * @todo #5102:90min Reduce long parameter lists in Linting, Parse, Pull, and similar classes.
     *  Linting currently takes 13 constructor parameters. Parse, Pull, and Probe have similar
     *  issues. The long parameter lists make call sites hard to read and fragile — adding a new
     *  option requires updating every call site across the codebase.
     * @checkstyle ParameterNumberCheck (20 lines)
     */
    @SuppressWarnings("PMD.ExcessiveParameterList")
    Linting(
        final TjsForeign srcs,
        final TjsForeign compiled,
        final Path target,
        final Path cache,
        final boolean enabled,
        final String ver,
        final Collection<String> sourcelints,
        final Collection<String> programlints,
        final boolean experimental,
        final boolean warning,
        final boolean pkg,
        final Path sources,
        final boolean skip
    ) {
        this.tojos = srcs;
        this.compile = compiled;
        this.targetDir = target;
        this.cacheDir = cache;
        this.cacheEnabled = enabled;
        this.version = ver;
        this.skipSourceLints = sourcelints;
        this.skipProgramLints = programlints;
        this.skipExperimentalLints = experimental;
        this.failOnWarning = warning;
        this.lintAsPackage = pkg;
        this.sourcesDir = sources;
        this.skipLinting = skip;
        this.guard = new ConcurrentCache();
    }

    @Override
    public void exec() throws IOException {
        if (this.skipLinting) {
            Logger.info(this, "Linting is skipped because eo:skipLinting is TRUE");
        } else {
            this.linting();
        }
    }

    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    private void linting() throws IOException {
        final Collection<TjForeign> programs = this.tojos.withXmir();
        final Map<Severity, Integer> counts = new ConcurrentHashMap<>();
        counts.putIfAbsent(Severity.CRITICAL, 0);
        counts.putIfAbsent(Severity.ERROR, 0);
        counts.putIfAbsent(Severity.WARNING, 0);
        final Collection<String> seen = new ConcurrentLinkedQueue<>();
        if (!this.skipSourceLints.isEmpty()) {
            Logger.info(this, "Unlinting source lints: %[list]s", this.skipSourceLints);
        }
        final int passed = new Threaded<>(
            programs,
            tojo -> this.lintOne(tojo, counts, seen, this.skipSourceLints.toArray(new String[0]))
        ).total();
        if (programs.isEmpty()) {
            Logger.info(this, "There are no XMIR programs, nothing to lint individually");
        }
        if (this.lintAsPackage) {
            Logger.info(
                this,
                "XMIR programs linted as a package: %d",
                this.lintAll(counts, seen)
            );
        } else {
            Logger.info(
                this,
                "Skipping linting as package (use -Deo.lintAsPackage=true to enable)"
            );
        }
        final String sum = Linting.summary(counts);
        Logger.info(
            this,
            "Linted %d out of %d XMIR program(s) that needed this (out of %d total programs): %s",
            passed, programs.size(), programs.size(), sum
        );
        Logger.info(
            this,
            "Read more about lints: https://www.objectionary.com/lints/%s",
            Manifests.read("Lints-Version")
        );
        final String details = String.join(System.lineSeparator(), seen);
        if (counts.get(Severity.ERROR) > 0 || counts.get(Severity.CRITICAL) > 0) {
            throw new IllegalStateException(
                String.format(
                    "In %d XMIR files, we found %s (must stop here):%n%s",
                    programs.size(), sum, details
                )
            );
        }
        if (counts.get(Severity.WARNING) > 0) {
            if (this.failOnWarning) {
                throw new IllegalStateException(
                    String.format(
                        "In %d XMIR files, we found %s (use -Deo.failOnWarning=false to ignore):%n%s",
                        programs.size(), sum, details
                    )
                );
            }
            Logger.info(
                this,
                "Use -Deo.failOnWarning=true to fail the build on warnings, currently it's set to false"
            );
        }
    }

    /**
     * XMIR verified to another XMIR.
     * @param tojo Foreign tojo
     * @param counts Counts of errors, warnings, and critical
     * @param seen Defects seen so far across all files
     * @param unlints Lints to skip
     * @return Amount of passed tojos (1 if passed, 0 if errors)
     * @throws Exception If failed to lint
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    private int lintOne(
        final TjForeign tojo,
        final Map<Severity, Integer> counts,
        final Collection<String> seen,
        final String... unlints
    ) throws Exception {
        final Path source = tojo.xmir();
        final XML xmir = new XMLDocument(source);
        final Path base = this.targetDir.resolve(Linting.DIR);
        final Path target = new Place(
            new OnDetailed(new OnDefault(new Xnav(xmir.inner())), source).get()
        ).make(base, MjAssemble.XMIR);
        if (this.cacheEnabled) {
            this.guard.apply(
                new Cache(
                    new CachePath(
                        this.cacheDir.resolve(Linting.CACHE),
                        this.version,
                        new TojoHash(tojo).get()
                    ),
                    src -> this.linted(
                        xmir,
                        unlints
                    ).toString()
                ),
                source, target, base.relativize(target)
            );
        } else {
            new Saved(
                this.linted(xmir, unlints).toString(),
                target
            ).value();
        }
        final Xnav checked = new Xnav(target);
        final Collection<Defect> defects = Linting.existing(checked);
        for (final Defect defect : defects) {
            if (Linting.notSuppressed(checked, defect)) {
                counts.compute(defect.severity(), (sev, before) -> before + 1);
                seen.add(
                    Linting.format(tojo.identifier(), defect.rule(), defect.line(), defect.text())
                );
                Linting.logOne(
                    defect.severity().mnemo(),
                    Linting.format(tojo.identifier(), defect.rule(), defect.line(), defect.text())
                );
            }
        }
        tojo.withLinted(target);
        return 1;
    }

    /**
     * Lint all XMIR files together.
     * @param counts Counts of errors, warnings, and critical
     * @param seen Defects seen so far across all files
     * @return Amount of seen XMIR files
     * @throws IOException If failed to lint
     */
    private int lintAll(
        final Map<Severity, Integer> counts,
        final Collection<String> seen
    ) throws IOException {
        final Map<String, Path> paths = new HashMap<>();
        for (final TjForeign tojo : this.tojos.withXmir()) {
            paths.put(tojo.identifier(), tojo.xmir());
        }
        for (final TjForeign tojo : this.compile.withXmir()) {
            paths.put(tojo.identifier(), tojo.xmir());
        }
        final Map<String, XML> pkg = new HashMap<>();
        for (final Map.Entry<String, Path> ent : paths.entrySet()) {
            pkg.put(ent.getKey(), new XMLDocument(ent.getValue()));
        }
        if (!this.skipProgramLints.isEmpty()) {
            Logger.info(this, "Unliting WPA lints: %[list]s", this.skipProgramLints);
        }
        final List<org.eolang.wpa.Defect> defects;
        if (this.cacheEnabled) {
            final Path wpa = Paths.get("wpa.xmir");
            final Path target = this.targetDir.resolve(Linting.DIR).resolve(wpa);
            new Cache(
                this.cacheDir.resolve(Linting.CACHE),
                root -> {
                    Logger.info(this, "Linting a package");
                    final Directives all = new Directives().add("defects");
                    for (final org.eolang.wpa.Defect defect : this.wpa(pkg)) {
                        Linting.embedded(all, defect);
                    }
                    all.up();
                    return new Xembler(all).xmlQuietly();
                },
                p -> p.getFileName().toString().endsWith(".xmir")
                    && !p.getFileName().equals(wpa)
            ).apply(this.sourcesDir, target, wpa);
            defects = Linting.read(target);
        } else {
            Logger.info(
                this,
                "Linting a package without cache, this might be slow, consider enabling cache"
            );
            defects = this.wpa(pkg);
        }
        for (final org.eolang.wpa.Defect defect : defects) {
            counts.compute(
                Severity.parsed(defect.severity().mnemo()), (sev, before) -> before + 1
            );
            seen.add(
                Linting.format(defect.object(), defect.rule(), defect.line(), defect.text())
            );
        }
        return pkg.size();
    }

    /**
     * Run whole-program analysis.
     * @param pkg Map of program identifiers to their XMIR
     * @return List of defects found
     */
    private List<org.eolang.wpa.Defect> wpa(final Map<String, XML> pkg) {
        final List<org.eolang.wpa.Defect> defects = new ArrayList<>(0);
        new Program(pkg)
            .without(this.skipProgramLints.toArray(new String[0]))
            .defects()
            .stream()
            .filter(defect -> this.skipExperimentalLints || !defect.experimental()).forEach(
                defect -> {
                    final Node node = pkg.get(defect.object()).inner();
                    new Xembler(
                        Linting.embedded(
                            new Directives().xpath("/object").addIf("errors").strict(1),
                            defect
                        )
                    ).applyQuietly(node);
                    if (Linting.notSuppressed(new Xnav(node), defect)) {
                        defects.add(defect);
                        Linting.logOne(
                            defect.severity().mnemo(),
                            Linting.format(
                                defect.object(), defect.rule(), defect.line(), defect.text()
                            )
                        );
                    }
                }
            );
        return defects;
    }

    /**
     * Find all possible linting defects and add them to the XMIR.
     * @param xmir The XML before linting
     * @param unlints Lints to skip
     * @return XML after linting
     * @checkstyle ParameterNumberCheck (40 lines)
     */
    private XML linted(final XML xmir, final String... unlints) {
        final Node node = xmir.inner();
        final Collection<Defect> defects = Linting.existing(new Xnav(node));
        final Collection<Defect> found = new Source(xmir)
            .without(unlints)
            .defects()
            .stream().filter(
                defect -> this.skipExperimentalLints || !defect.experimental()
            ).collect(Collectors.toList());
        defects.addAll(found);
        final Directives dirs = new Directives();
        if (!found.isEmpty()) {
            dirs.xpath("/object").addIf("errors").strict(1);
        }
        for (final Defect defect : defects) {
            if (found.contains(defect)) {
                Linting.embedded(dirs, defect);
            }
        }
        new Xembler(dirs).applyQuietly(node);
        return new XMLDocument(node);
    }

    /**
     * Log one defect message.
     * @param severity Severity mnemo (e.g. "warning", "error")
     * @param message Formatted defect message
     */
    private static void logOne(final String severity, final String message) {
        if (Severity.WARNING.mnemo().equals(severity)) {
            Logger.warn(Linting.class, "[LINT] %s", message);
        } else {
            Logger.error(Linting.class, "[LINT] %s", message);
        }
    }

    /**
     * Text in plural or singular form.
     * @param count Count
     * @param name Name of them
     * @return Summary text
     */
    private static String plural(final int count, final String name) {
        final StringBuilder txt = new StringBuilder();
        txt.append(count).append(' ').append(name);
        if (count > 1) {
            txt.append('s');
        }
        return txt.toString();
    }

    /**
     * Summarize the counts.
     * @param counts Counts of errors, warnings, and critical
     * @return Summary text
     */
    private static String summary(final Map<Severity, Integer> counts) {
        final List<String> parts = new ArrayList<>(0);
        final int critical = counts.get(Severity.CRITICAL);
        if (critical > 0) {
            parts.add(Linting.plural(critical, "critical error"));
        }
        final int errors = counts.get(Severity.ERROR);
        if (errors > 0) {
            parts.add(Linting.plural(errors, "error"));
        }
        final int warnings = counts.get(Severity.WARNING);
        if (warnings > 0) {
            parts.add(Linting.plural(warnings, "warning"));
        }
        if (parts.isEmpty()) {
            parts.add("no complaints");
        }
        final String sum;
        if (parts.size() < 3) {
            sum = String.join(" and ", parts);
        } else {
            sum = String.format(
                "%s, and %s",
                String.join(", ", parts.subList(0, parts.size() - 2)),
                parts.get(parts.size() - 1)
            );
        }
        return sum;
    }

    /**
     * Collection of defects existing in XMIR before linting.
     * @param xnav XML node as Xnav
     * @return Collection of defects
     */
    private static Collection<Defect> existing(final Xnav xnav) {
        return xnav
            .element("object")
            .elements(Filter.withName("errors"))
            .findFirst().map(
                errors -> errors
                    .elements(Filter.withName("error"))
                    .map(Linting::toDefect)
                    .collect(Collectors.toList())
            )
            .orElse(new ListOf<>());
    }

    /**
     * Convert XMIR error element to a {@link Defect}.
     * @param error The error element
     * @return Defect
     */
    private static Defect toDefect(final Xnav error) {
        return new Defect.Default(
            error.attribute("check").text().orElseThrow(
                () -> new IllegalArgumentException(
                    "The <error> element in XMIR must contain 'check' attribute"
                )
            ),
            Severity.parsed(
                error.attribute("severity").text().orElseThrow(
                    () -> new IllegalArgumentException(
                        "The <error> element in XMIR must contain 'severity' attribute"
                    )
                )
            ),
            Integer.parseInt(
                error.attribute("line").text().orElse("0")
            ),
            error.text().orElseThrow(
                () -> new IllegalStateException(
                    "The <error> element in XMIR must contain text message"
                )
            )
        );
    }

    /**
     * Inject defect into XMIR.
     * @param dirs Directives
     * @param defect The defect to inject
     * @return Directives
     */
    private static Directives embedded(final Directives dirs, final Defect defect) {
        dirs.add("error")
            .attr("check", defect.rule())
            .attr("severity", defect.severity().mnemo())
            .set(defect.text());
        if (defect.line() > 0) {
            dirs.attr("line", defect.line());
        }
        return dirs.up();
    }

    private static Directives embedded(
        final Directives dirs, final org.eolang.wpa.Defect defect
    ) {
        dirs.add("error")
            .attr("check", defect.rule())
            .attr("severity", defect.severity().mnemo())
            .set(defect.text());
        if (defect.line() > 0) {
            dirs.attr("line", defect.line());
        }
        return dirs.up();
    }

    private static boolean notSuppressed(
        final Xnav xnav, final org.eolang.wpa.Defect defect
    ) {
        return xnav.path(
            String.format("/object/metas/meta[head='unlint' and tail='%s']", defect.rule())
        ).findAny().isEmpty();
    }

    /**
     * Format a defect for display.
     * @param object Program or object identifier
     * @param rule Rule name
     * @param line Line number
     * @param text Defect message
     * @return Formatted string
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    private static String format(
        final String object, final String rule, final int line, final String text
    ) {
        return String.format("%s:%d %s (%s)", object, line, text, rule);
    }

    /**
     * Read defects from XMIR.
     * @param path Path to XMIR
     * @return Collection of defects
     */
    private static List<org.eolang.wpa.Defect> read(final Path path) {
        return new Xnav(path).path("/defects/error").map(
            node -> new org.eolang.wpa.Defect.Default(
                node.attribute("check").text().orElseThrow(),
                org.eolang.wpa.Severity.parsed(
                    node.attribute("severity").text().orElseThrow()
                ),
                node.attribute("object").text().orElse(""),
                Integer.parseInt(node.attribute("line").text().orElse("0")),
                node.text().orElse("")
            )
        ).collect(Collectors.toList());
    }

    /**
     * This defect is not suppressed?
     * @param xnav The XMIR as {@link Xnav}
     * @param defect The defect
     * @return TRUE if not suppressed
     */
    private static boolean notSuppressed(final Xnav xnav, final Defect defect) {
        return xnav.path(
            String.format(
                "/object/metas/meta[head='unlint' and tail='%s']",
                Linting.baseRule(defect.rule())
            )
        ).findAny().isEmpty();
    }

    /**
     * Strip scope suffix (e.g. {@code /S}, {@code /W}) from a scoped rule name.
     * @param rule Rule name, possibly scoped
     * @return Base rule name without scope suffix
     */
    private static String baseRule(final String rule) {
        final int slash = rule.lastIndexOf('/');
        final String result;
        if (slash >= 0) {
            result = rule.substring(0, slash);
        } else {
            result = rule;
        }
        return result;
    }
}
