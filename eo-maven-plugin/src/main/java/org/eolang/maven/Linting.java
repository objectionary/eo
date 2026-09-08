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
import java.nio.file.Files;
import java.nio.file.Path;
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
import org.eolang.wpa.Program;
import org.w3c.dom.Node;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Runs all lints and checks errors and warnings.
 *
 * <p>This class goes through all XMIR files generated in the previous steps (see {@link MjParse}
 * or {@link MjPull} goals) and runs all available lints on them.
 * If any errors or warnings are found, they are logged to the console,
 * and depending on the configuration, the build may fail.
 * The linting results are also embedded back into the XMIR files for future reference.
 * Lints might use caching to speed up the process on subsequent runs.
 * Cached files are stored in the {@link #CACHE} directory.
 * The results of linting are saved in the {@link #DIR} directory.</p>
 *
 * <p>Note: this class is intentionally named {@code Linting} rather than {@code Lint} to avoid
 * a conflict with Maven's Plexus configurator. When a class named {@code Lint} exists in the
 * plugin package, Plexus tries to instantiate it (via no-arg constructor) as the element type
 * for any {@code lint} XML child element it encounters in plugin configuration.
 * Naming the class {@code Linting} avoids this collision.</p>
 *
 * @since 0.31.0
 */
@SuppressWarnings("PMD.GodClass")
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
     * The XMIR {@code object} element/attribute name, used both for
     * navigating a source XMIR and for round-tripping a WPA defect's
     * program name through {@code wpa.xmir}.
     */
    private static final String OBJECT = "object";

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
     */
    private final Path target;

    /**
     * Cache base directory.
     */
    private final Path cache;

    /**
     * Whether caching is enabled.
     */
    private final boolean enabled;

    /**
     * Plugin version.
     */
    private final String version;

    /**
     * Source lints to skip.
     */
    private final Collection<String> sourcelints;

    /**
     * Program (WPA) lints to skip.
     */
    private final Collection<String> programlints;

    /**
     * Whether to skip experimental lints.
     */
    private final boolean experimental;

    /**
     * Whether to fail on warnings.
     */
    private final boolean warning;

    /**
     * Whether to lint all sources as a package (WPA).
     */
    private final boolean pkg;

    /**
     * Whether to skip linting entirely.
     */
    private final boolean skip;

    /**
     * Cache guard, see {@link ConcurrentCache} for why it is one per instance.
     */
    private final ConcurrentCache guard;

    /**
     * Constructor.
     *
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
     * @param skip Whether to skip linting entirely
     */
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
        final boolean skip
    ) {
        this.tojos = srcs;
        this.compile = compiled;
        this.target = target;
        this.cache = cache;
        this.enabled = enabled;
        this.version = ver;
        this.sourcelints = sourcelints;
        this.programlints = programlints;
        this.experimental = experimental;
        this.warning = warning;
        this.pkg = pkg;
        this.skip = skip;
        this.guard = new ConcurrentCache();
    }

    @Override
    public void exec() throws IOException {
        if (this.skip) {
            Logger.info(this, "Linting is skipped because eo:skipLinting is TRUE");
        } else {
            this.linting();
        }
    }

    /**
     * Summarize the counts.
     *
     * @param counts Counts of errors, warnings, and critical
     * @return Summary text
     */
    static String summary(final Map<Severity, Integer> counts) {
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
                String.join(", ", parts.subList(0, parts.size() - 1)),
                parts.get(parts.size() - 1)
            );
        }
        return sum;
    }

    private void linting() throws IOException {
        final Collection<TjForeign> programs = this.tojos.withXmir();
        final Map<Severity, Integer> counts = new ConcurrentHashMap<>();
        counts.putIfAbsent(Severity.CRITICAL, 0);
        counts.putIfAbsent(Severity.ERROR, 0);
        counts.putIfAbsent(Severity.WARNING, 0);
        final Collection<String> seen = new ConcurrentLinkedQueue<>();
        if (!this.sourcelints.isEmpty()) {
            Logger.info(this, "Unlinting source lints: %[list]s", this.sourcelints);
        }
        final int passed = new Threaded<>(
            programs,
            tojo -> this.lintOne(tojo, counts, seen)
        ).total();
        if (programs.isEmpty()) {
            Logger.info(this, "There are no XMIR programs, nothing to lint individually");
        }
        if (this.pkg) {
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
            if (this.warning) {
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

    private int lintOne(
        final TjForeign tojo,
        final Map<Severity, Integer> counts,
        final Collection<String> seen
    ) throws Exception {
        final Path source = tojo.xmir();
        final XML xmir = new XMLDocument(source);
        final Path base = this.target.resolve(Linting.DIR);
        final Path out = new LintTarget(xmir, source).under(base);
        if (this.enabled) {
            this.guard.apply(
                source, out, base.relativize(out),
                new Cache(
                    new CachePath(
                        this.cache.resolve(Linting.CACHE),
                        this.cacheVersion(),
                        new TojoHash(tojo).get()
                    ),
                    src -> this.linted(xmir).toString()
                )
            );
        } else {
            new Saved(
                this.linted(xmir).toString(),
                out
            ).value();
        }
        final Xnav checked = new Xnav(out);
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
        tojo.withLinted(out);
        return 1;
    }

    private String cacheVersion() {
        return String.format(
            "%s-%s-%b-%s",
            this.version,
            Manifests.read("Lints-Version"),
            this.experimental,
            new Hashed(
                this.sourcelints.stream().sorted().collect(Collectors.joining(","))
            ).get()
        );
    }

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
        final Map<String, XML> progs = new HashMap<>();
        for (final Map.Entry<String, Path> ent : paths.entrySet()) {
            progs.put(ent.getKey(), new XMLDocument(ent.getValue()));
        }
        if (!this.programlints.isEmpty()) {
            Logger.info(this, "Unliting WPA lints: %[list]s", this.programlints);
        }
        final List<org.eolang.wpa.Defect> defects;
        if (this.enabled) {
            final Path wpa = Path.of("wpa.xmir");
            final Path base = this.target.resolve(Linting.DIR);
            final Path out = base.resolve(wpa);
            Files.createDirectories(base);
            this.guard.apply(
                base, out, wpa,
                new Cache(
                    new CachePath(
                        this.cache.resolve(Linting.CACHE),
                        this.version,
                        new WpaCacheKey(
                            paths, this.programlints, this.experimental,
                            Manifests.read("Wpa-Version")
                        ).get()
                    ).get(),
                    root -> {
                        Logger.info(this, "Linting a package");
                        final Directives all = new Directives().add("defects");
                        for (final org.eolang.wpa.Defect defect : this.wpa(progs)) {
                            Linting.embedded(all, defect);
                        }
                        all.up();
                        return new Xembler(all).xmlQuietly();
                    },
                    p -> p.getFileName().toString().endsWith(".xmir")
                        && !p.equals(out)
                )
            );
            defects = Linting.read(out);
        } else {
            Logger.info(
                this,
                "Linting a package without cache, this might be slow, consider enabling cache"
            );
            defects = this.wpa(progs);
        }
        for (final org.eolang.wpa.Defect defect : defects) {
            counts.compute(
                Severity.parsed(defect.severity().mnemo()), (sev, before) -> before + 1
            );
            final String message = Linting.format(
                defect.object(), defect.rule(), defect.line(), defect.text()
            );
            seen.add(message);
            Linting.logOne(defect.severity().mnemo(), message);
        }
        return progs.size();
    }

    private List<org.eolang.wpa.Defect> wpa(final Map<String, XML> progs) {
        final List<org.eolang.wpa.Defect> defects = new ArrayList<>(0);
        new Program(progs)
            .without(this.programlints.toArray(new String[0]))
            .defects()
            .stream()
            .filter(defect -> !(this.experimental && defect.experimental())).forEach(
                defect -> {
                    final Node node = progs.get(defect.object()).inner();
                    new Xembler(
                        Linting.embedded(
                            new Directives().xpath("/object").addIf("errors").strict(1),
                            defect
                        )
                    ).applyQuietly(node);
                    if (Linting.notSuppressed(new Xnav(node), defect)) {
                        defects.add(defect);
                    }
                }
            );
        return defects;
    }

    private XML linted(final XML xmir) {
        final Node node = xmir.inner();
        final Collection<Defect> defects = Linting.existing(new Xnav(node));
        final Collection<Defect> found = new Source(xmir)
            .without(this.sourcelints.toArray(new String[0]))
            .defects()
            .stream().filter(
                defect -> !(this.experimental && defect.experimental())
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

    private static void logOne(final String severity, final String message) {
        if (Severity.WARNING.mnemo().equals(severity)) {
            Logger.warn(Linting.class, "[LINT] %s", message);
        } else {
            Logger.error(Linting.class, "[LINT] %s", message);
        }
    }

    private static String plural(final int count, final String name) {
        final StringBuilder txt = new StringBuilder();
        txt.append(count).append(' ').append(name);
        if (count > 1) {
            txt.append('s');
        }
        return txt.toString();
    }

    private static Collection<Defect> existing(final Xnav xnav) {
        return xnav
            .element(Linting.OBJECT)
            .elements(Filter.withName("errors"))
            .findFirst().map(
                errors -> errors
                    .elements(Filter.withName("error"))
                    .map(Linting::toDefect)
                    .collect(Collectors.toList())
            )
            .orElse(new ListOf<>());
    }

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
            .attr(Linting.OBJECT, defect.object())
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
            String.format(
                "/object/metas/meta[head='unlint' and tail='%s']",
                Linting.baseRule(defect.rule())
            )
        ).findAny().isEmpty();
    }

    private static String format(
        final String object, final String rule, final int line, final String text
    ) {
        return String.format("%s:%d %s (%s)", object, line, text, rule);
    }

    private static List<org.eolang.wpa.Defect> read(final Path path) {
        return new Xnav(path).path("/defects/error").map(
            node -> new org.eolang.wpa.Defect.Default(
                node.attribute("check").text().orElseThrow(),
                org.eolang.wpa.Severity.parsed(
                    node.attribute("severity").text().orElseThrow()
                ),
                node.attribute(Linting.OBJECT).text().orElse(""),
                Integer.parseInt(node.attribute("line").text().orElse("0")),
                node.text().orElse("")
            )
        ).collect(Collectors.toList());
    }

    private static boolean notSuppressed(final Xnav xnav, final Defect defect) {
        return xnav.path(
            String.format(
                "/object/metas/meta[head='unlint' and tail='%s']",
                Linting.baseRule(defect.rule())
            )
        ).findAny().isEmpty();
    }

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
