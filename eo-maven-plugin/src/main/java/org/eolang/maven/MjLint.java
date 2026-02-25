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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.list.ListOf;
import org.eolang.lints.Defect;
import org.eolang.lints.Program;
import org.eolang.lints.Severity;
import org.eolang.lints.Source;
import org.eolang.parser.OnDefault;
import org.eolang.parser.OnDetailed;
import org.w3c.dom.Node;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Mojo that runs all lints and checks errors and warnings,
 * preferably after the {@code assemble} goal.
 * <p>
 *     This goal goes through all XMIR files generated in the previous steps (see {@link MjParse}
 *     or {@link MjPull} goals) and runs all available lints on them.
 *     If any errors or warnings are found, they are logged to the console,
 *     and depending on the configuration, the build may fail.
 *     The linting results are also embedded back into the XMIR files for future reference.
 *     Lints might use caching to speed up the process on subsequent runs.
 *     Cached files are stored in the {@link #CACHE} directory.
 *     The results of linting are saved in the {@link #DIR} directory.
 * </p>
 * @since 0.31.0
 */
@Mojo(
    name = "lint",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
@SuppressWarnings("PMD.TooManyMethods")
public final class MjLint extends MjSafe {
    /**
     * The directory where to transpile to.
     */
    static final String DIR = "3-lint";

    /**
     * Subdirectory for optimized cache.
     */
    static final String CACHE = "linted";

    @Override
    void exec() throws IOException {
        if (this.skipLinting) {
            Logger.info(this, "Linting is skipped because eo:skipLinting is TRUE");
        } else {
            this.lint();
        }
    }

    /**
     * Lint.
     * @throws IOException If fails
     */
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    private void lint() throws IOException {
        final long start = System.currentTimeMillis();
        final Collection<TjForeign> tojos = this.scopedTojos().withXmir();
        final Map<Severity, Integer> counts = new ConcurrentHashMap<>();
        counts.putIfAbsent(Severity.CRITICAL, 0);
        counts.putIfAbsent(Severity.ERROR, 0);
        counts.putIfAbsent(Severity.WARNING, 0);
        if (!this.skipSourceLints.isEmpty()) {
            Logger.info(this, "Unlinting source lints: %[list]s", this.skipSourceLints);
        }
        final int passed = new Threaded<>(
            tojos, tojo -> this.lintOne(tojo, counts, this.skipSourceLints.toArray(new String[0]))
        ).total();
        if (tojos.isEmpty()) {
            Logger.info(this, "There are no XMIR programs, nothing to lint individually");
        }
        if (this.lintAsPackage) {
            Logger.info(
                this,
                "XMIR programs linted as a package: %d",
                this.lintAll(counts)
            );
        } else {
            Logger.info(
                this,
                "Skipping linting as package (use -Deo.lintAsPackage=true to enable)"
            );
        }
        final String sum = MjLint.summary(counts);
        Logger.info(
            this,
            "Linted %d out of %d XMIR program(s) that needed this (out of %d total programs) in %[ms]s: %s",
            passed, tojos.size(), tojos.size(), System.currentTimeMillis() - start, sum
        );
        Logger.info(
            this,
            "Read more about lints: https://www.objectionary.com/lints/%s",
            Manifests.read("Lints-Version")
        );
        if (counts.get(Severity.ERROR) > 0 || counts.get(Severity.CRITICAL) > 0) {
            throw new IllegalStateException(
                String.format(
                    "In %d XMIR files, we found %s (must stop here)",
                    tojos.size(), sum
                )
            );
        } else if (counts.get(Severity.WARNING) > 0 && this.failOnWarning) {
            throw new IllegalStateException(
                String.format(
                    "In %d XMIR files, we found %s (use -Deo.failOnWarning=false to ignore)",
                    tojos.size(), sum
                )
            );
        }
    }

    /**
     * XMIR verified to another XMIR.
     * @param tojo Foreign tojo
     * @param counts Counts of errors, warnings, and critical
     * @param unlints Lints to skip
     * @return Amount of passed tojos (1 if passed, 0 if errors)
     * @throws Exception If failed to lint
     */
    private int lintOne(
        final TjForeign tojo,
        final Map<Severity, Integer> counts,
        final String... unlints
    ) throws Exception {
        final Path source = tojo.xmir();
        final XML xmir = new XMLDocument(source);
        final Path base = this.targetDir.toPath().resolve(MjLint.DIR);
        final Path target = new Place(
            new OnDetailed(new OnDefault(xmir), source).get()
        ).make(base, MjAssemble.XMIR);
        if (this.cacheEnabled) {
            new ConcurrentCache(
                new Cache(
                    new CachePath(
                        this.cache.toPath().resolve(MjLint.CACHE),
                        this.plugin.getVersion(),
                        new TojoHash(tojo).get()
                    ),
                    src -> this.linted(
                        tojo.identifier(),
                        xmir,
                        counts,
                        unlints
                    ).toString()
                )
            ).apply(source, target, base.relativize(target));
        } else {
            new Saved(
                this.linted(tojo.identifier(), xmir, counts, unlints)
                    .toString(),
                target
            ).value();
        }
        tojo.withLinted(target);
        return 1;
    }

    /**
     * Lint all XMIR files together.
     * @param counts Counts of errors, warnings, and critical
     * @return Amount of seen XMIR files
     * @throws IOException If failed to lint
     */
    private int lintAll(final Map<Severity, Integer> counts) throws IOException {
        final Map<String, Path> paths = new HashMap<>();
        for (final TjForeign tojo : this.scopedTojos().withXmir()) {
            paths.put(tojo.identifier(), tojo.xmir());
        }
        for (final TjForeign tojo : this.compileTojos().withXmir()) {
            paths.put(tojo.identifier(), tojo.xmir());
        }
        final Map<String, XML> pkg = new HashMap<>();
        for (final Map.Entry<String, Path> ent : paths.entrySet()) {
            pkg.put(ent.getKey(), new XMLDocument(ent.getValue()));
        }
        if (!this.skipProgramLints.isEmpty()) {
            Logger.info(this, "Unliting WPA lints: %[list]s", this.skipProgramLints);
        }
        new Program(pkg)
            .without(this.skipProgramLints.toArray(new String[0]))
            .defects()
            .stream()
            .filter(defect -> this.skipExperimentalLints || !defect.experimental())
            .forEach(
                defect -> {
                    final Node node = pkg.get(defect.object()).inner();
                    new Xembler(
                        MjLint.embedded(
                            new Directives().xpath("/object").addIf("errors").strict(1),
                            defect
                        )
                    ).applyQuietly(node);
                    if (MjLint.notSuppressed(new Xnav(node), defect)) {
                        counts.compute(defect.severity(), (sev, before) -> before + 1);
                        MjLint.logOne(defect);
                    }
                }
            );
        return pkg.size();
    }

    /**
     * Find all possible linting defects and add them to the XMIR.
     * @param program Program identifier
     * @param xmir The XML before linting
     * @param counts Counts of errors, warnings, and critical
     * @param unlints Lints to skip
     * @return XML after linting
     * @checkstyle ParameterNumberCheck (40 lines)
     */
    private XML linted(
        final String program,
        final XML xmir,
        final Map<Severity, Integer> counts,
        final String... unlints
    ) {
        final Node node = xmir.inner();
        final Xnav xnav = new Xnav(node);
        final Collection<Defect> defects = MjLint.existing(program, xnav);
        final Collection<Defect> found = new Source(xmir)
            .without(unlints)
            .defects()
            .stream()
            .filter(
                defect -> this.skipExperimentalLints || !defect.experimental()
            ).collect(Collectors.toList());
        defects.addAll(found);
        final Directives dirs = new Directives();
        if (!found.isEmpty()) {
            dirs.xpath("/object").addIf("errors").strict(1);
        }
        for (final Defect defect : defects) {
            if (found.contains(defect)) {
                MjLint.embedded(dirs, defect);
            }
            if (MjLint.notSuppressed(xnav, defect)) {
                counts.compute(defect.severity(), (sev, before) -> before + 1);
                MjLint.logOne(defect);
            }
        }
        new Xembler(dirs).applyQuietly(node);
        return new XMLDocument(node);
    }

    /**
     * Log one defect.
     * @param defect The defect to log
     */
    private static void logOne(final Defect defect) {
        final StringBuilder message = new StringBuilder()
            .append(defect.object())
            .append(':').append(defect.line())
            .append(' ')
            .append(defect.text())
            .append(" (")
            .append(defect.rule())
            .append(')');
        switch (defect.severity()) {
            case WARNING:
                Logger.warn(MjLint.class, message.toString());
                break;
            case ERROR:
            case CRITICAL:
                Logger.error(MjLint.class, message.toString());
                break;
            default:
                throw new IllegalArgumentException(
                    String.format(
                        "Not yet supported severity: %s",
                        defect.severity()
                    )
                );
        }
    }

    /**
     * Text in plural or singular form.
     * @param count Counts of errors, warnings, and critical
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
            parts.add(MjLint.plural(critical, "critical error"));
        }
        final int errors = counts.get(Severity.ERROR);
        if (errors > 0) {
            parts.add(MjLint.plural(errors, "error"));
        }
        final int warnings = counts.get(Severity.WARNING);
        if (warnings > 0) {
            parts.add(MjLint.plural(warnings, "warning"));
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
     * @param program Program name
     * @param xnav XML node as Xnav
     * @return Collection of defects
     */
    private static Collection<Defect> existing(final String program, final Xnav xnav) {
        return xnav
            .element("object")
            .elements(Filter.withName("errors"))
            .findFirst()
            .map(
                errors -> errors
                    .elements(Filter.withName("error"))
                    .map(
                        error -> (Defect) new Defect.Default(
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
                            program,
                            Integer.parseInt(
                                error.attribute("line").text().orElse("0")
                            ),
                            error.text().orElseThrow(
                                () -> new IllegalStateException(
                                    "The <error> element in XMIR must contain text message"
                                )
                            )
                        )
                    )
                    .collect(Collectors.toList())
            )
            .orElse(new ListOf<>());
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

    /**
     * This defect is not suppressed?
     * @param xnav The XMIR as {@link Xnav}
     * @param defect The defect
     * @return TRUE if not suppressed
     */
    private static boolean notSuppressed(final Xnav xnav, final Defect defect) {
        return xnav.path(
            String.format("/object/metas/meta[head='unlint' and tail='%s']", defect.rule())
        ).findAny().isEmpty();
    }
}
