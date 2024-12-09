/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Filtered;
import org.cactoos.list.ListOf;
import org.eolang.lints.Defect;
import org.eolang.lints.Program;
import org.eolang.lints.Programs;
import org.eolang.lints.Severity;
import org.eolang.maven.footprint.FpDefault;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.tojos.TojoHash;
import org.eolang.maven.util.Threaded;
import org.w3c.dom.Node;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Mojo that runs all lints and checks errors and warnings,
 * preferably after the {@code assemble} goal.
 *
 * @since 0.31.0
 */
@Mojo(
    name = "lint",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class LintMojo extends SafeMojo {
    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "6-lint";

    /**
     * Subdirectory for optimized cache.
     */
    static final String CACHE = "linted";

    /**
     * Whether we should fail on warning.
     *
     * @checkstyle MemberNameCheck (11 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(
        property = "eo.failOnWarning",
        required = true,
        defaultValue = "false"
    )
    private boolean failOnWarning;

    @Override
    void exec() throws IOException {
        final Collection<ForeignTojo> tojos = this.scopedTojos().withShaken();
        final ConcurrentHashMap<Severity, Integer> counts = new ConcurrentHashMap<>();
        counts.putIfAbsent(Severity.CRITICAL, 0);
        counts.putIfAbsent(Severity.ERROR, 0);
        counts.putIfAbsent(Severity.WARNING, 0);
        final Collection<ForeignTojo> must = new ListOf<>(
            new Filtered<>(
                ForeignTojo::notLinted,
                tojos
            )
        );
        final int passed = new Threaded<>(
            must,
            tojo -> this.lintOne(tojo, counts)
        ).total();
        if (must.isEmpty()) {
            Logger.info(
                this,
                "No XMIR programs out of %d linted individually",
                tojos.size()
            );
        } else if (tojos.isEmpty()) {
            Logger.info(this, "There are no XMIR programs, nothing to lint individually");
        }
        Logger.info(
            this,
            "Also, %d XMIR programs linted as a package",
            this.lintAll(counts)
        );
        if (counts.get(Severity.ERROR) > 0 || counts.get(Severity.CRITICAL) > 0
            || counts.get(Severity.WARNING) > 0 && this.failOnWarning) {
            final String sum = LintMojo.summary(counts);
            Logger.info(
                this,
                "Linted %d out of %d XMIR program(s) that needed this (out of %d total programs): %s",
                passed, must.size(), tojos.size(), sum
            );
            throw new IllegalStateException(
                String.format("In %d XMIR files, we found %s", must.size(), sum)
            );
        } else {
            Logger.info(
                this,
                "Linted %d out of %d XMIR program(s) that needed this (out of %d total programs), no problems found",
                passed, must.size(), tojos.size()
            );
        }
    }

    /**
     * XMIR verified to another XMIR.
     * @param tojo Foreign tojo
     * @param counts Counts of errors, warnings, and critical
     * @return Amount of passed tojos (1 if passed, 0 if errors)
     * @throws Exception If failed to lint
     */
    private int lintOne(final ForeignTojo tojo,
        final ConcurrentHashMap<Severity, Integer> counts) throws Exception {
        final Path source = tojo.shaken();
        final XML xmir = new XMLDocument(source);
        final String name = xmir.xpath("/program/@name").get(0);
        final Path base = this.targetDir.toPath().resolve(LintMojo.DIR);
        final Path target = new Place(name).make(base, AssembleMojo.XMIR);
        tojo.withLinted(
            new FpDefault(
                src -> this.counted(LintMojo.lint(xmir), counts).toString(),
                this.cache.toPath().resolve(LintMojo.CACHE),
                this.plugin.getVersion(),
                new TojoHash(tojo),
                base.relativize(target)
            ).apply(source, target)
        );
        return 1;
    }

    /**
     * Lint all XMIR files together.
     * @param counts Counts of errors, warnings, and critical
     * @return Amount of seen XMIR files
     * @throws IOException If failed to lint
     */
    private int lintAll(final ConcurrentHashMap<Severity, Integer> counts) throws IOException {
        final Map<String, Path> paths = new HashMap<>();
        for (final ForeignTojo tojo : this.scopedTojos().withLinted()) {
            paths.put(tojo.identifier(), tojo.linted());
        }
        final Map<String, XML> pkg = new HashMap<>();
        for (final Map.Entry<String, Path> ent : paths.entrySet()) {
            pkg.put(ent.getKey(), new XMLDocument(ent.getValue()));
        }
        final Collection<Defect> defects = new Programs(pkg).defects();
        for (final Defect defect : defects) {
            counts.compute(defect.severity(), (sev, before) -> before + 1);
            LintMojo.embed(
                pkg.get(defect.program()),
                new ListOf<>(defect)
            );
        }
        return pkg.size();
    }

    /**
     * Log errors of XMIR.
     * @param xml XMIR.
     * @param counts Counts of errors, warnings, and critical
     * @return TRUE if it's good
     */
    private XML counted(final XML xml, final ConcurrentHashMap<Severity, Integer> counts) {
        for (final XML error : xml.nodes("/program/errors/error")) {
            final List<String> line = error.xpath("@line");
            final StringBuilder message = new StringBuilder()
                .append(Logger.format("%[file]s", xml.xpath("/program/@source").get(0)));
            if (!line.isEmpty()) {
                message.append(':').append(Integer.parseInt(line.get(0)));
            }
            message.append(' ')
                .append(error.xpath("text()").get(0))
                .append(" (")
                .append(error.xpath("@check").get(0))
                .append(' ')
                .append(error.xpath("@severity").get(0))
                .append(')');
            final Severity severity = Severity.parsed(error.xpath("@severity").get(0));
            counts.compute(severity, (sev, before) -> before + 1);
            switch (severity) {
                case WARNING:
                    Logger.warn(this, message.toString());
                    break;
                case ERROR:
                case CRITICAL:
                    Logger.error(this, message.toString());
                    break;
                default:
                    throw new IllegalArgumentException(
                        String.format("Not yet supported severity: %s", severity)
                    );
            }
        }
        return xml;
    }

    /**
     * Summarize the counts.
     * @param counts Counts of errors, warnings, and critical
     * @return Summary text
     */
    private static String summary(final ConcurrentHashMap<Severity, Integer> counts) {
        final StringBuilder sum = new StringBuilder(100);
        final int criticals = counts.get(Severity.CRITICAL);
        if (criticals > 0) {
            sum.append(criticals).append(" critical error");
            if (criticals > 1) {
                sum.append('s');
            }
        }
        final int errors = counts.get(Severity.ERROR);
        if (errors > 0) {
            if (sum.length() > 0) {
                sum.append(", ");
            }
            sum.append(errors).append(" error");
            if (errors > 1) {
                sum.append('s');
            }
        }
        final int warnings = counts.get(Severity.WARNING);
        if (warnings > 0) {
            if (sum.length() > 0) {
                sum.append(", ");
            }
            sum.append(warnings).append(" warning");
            if (warnings > 1) {
                sum.append('s');
            }
        }
        if (sum.length() == 0) {
            sum.append("no complaints");
        }
        return sum.toString();
    }

    /**
     * Find all possible linting defects and add them to the XMIR.
     * @param xmir The XML before linting
     * @return XML after linting
     * @throws IOException If fails
     */
    private static XML lint(final XML xmir) throws IOException {
        final Directives dirs = new Directives();
        final Collection<Defect> defects = new Program(xmir).defects();
        if (!defects.isEmpty()) {
            dirs.xpath("/program").addIf("errors").strict(1);
            LintMojo.embed(xmir, defects);
        }
        final Node node = xmir.inner();
        new Xembler(dirs).applyQuietly(node);
        return new XMLDocument(node);
    }

    /**
     * Inject defect into XMIR.
     * @param xmir The XML before linting
     * @param defects The defects to inject
     */
    private static void embed(final XML xmir, final Collection<Defect> defects) {
        final Directives dirs = new Directives();
        dirs.xpath("/program").addIf("errors").strict(1);
        for (final Defect defect : defects) {
            if (LintMojo.suppressed(xmir, defect)) {
                continue;
            }
            dirs.add("error")
                .attr("check", defect.rule())
                .attr("severity", defect.severity().mnemo())
                .set(defect.text());
            if (defect.line() > 0) {
                dirs.attr("line", defect.line());
            }
            dirs.up();
        }
        final Node node = xmir.inner();
        new Xembler(dirs).applyQuietly(node);
    }

    /**
     * This defect is suppressed?
     * @param xmir The XMIR
     * @param defect The defect
     * @return TRUE if suppressed
     */
    private static boolean suppressed(final XML xmir, final Defect defect) {
        return !xmir.nodes(
            String.format(
                "/program/metas/meta[head='unlint' and tail='%s']",
                defect.rule()
            )
        ).isEmpty();
    }

}
