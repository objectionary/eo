/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StBefore;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StEndless;
import com.yegor256.xsline.StSchema;
import com.yegor256.xsline.StXSL;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrFast;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.TrLogged;
import com.yegor256.xsline.TrMapped;
import com.yegor256.xsline.TrWith;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.StringJoiner;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.io.ResourceOf;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Rel;
import org.eolang.parser.StXPath;
import org.xembly.Directive;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Convert XMIR to SODG.
 *
 * SODG (Surging Object DiGraph) is our own format of graph representation.
 * It essentially is a text file that consists of instructions for a virtual
 * machine that is capable of parsing them and building a graph. An example
 * of such a machine can be found in
 * <a href="https://github.com/objectionary/sodg">this repository</a>. When the
 * graph is built by the virtual machine, it must be possible to execute
 * a program using graph traversing algorithm. An example of such an executor
 * of a graph can be found in
 * <a href="https://github.com/objectionary/reo">this repository</a>.
 *
 * @since 0.27
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 */
@Mojo(
    name = "sodg",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.ImmutableField")
public final class SodgMojo extends SafeMojo {

    /**
     * The directory where to save SODG to.
     */
    public static final String DIR = "sodg";

    /**
     * SODG to plain text.
     */
    private static final Train<Shift> TO_TEXT = new TrFast(
        new TrClasspath<>(
            "/org/eolang/maven/sodg-to/normalize-names.xsl",
            "/org/eolang/maven/sodg-to/to-text.xsl"
        ).back(),
        SodgMojo.class
    );

    /**
     * SODG to Xembly.
     */
    private static final Train<Shift> TO_XEMBLY = new TrFast(
        new TrDefault<Shift>().with(
            new StXSL(
                new XSLDocument(
                    new UncheckedText(
                        new TextOf(
                            new ResourceOf(
                                "org/eolang/maven/sodg-to/to-xembly.xsl"
                            )
                        )
                    ).asString(),
                    new ClasspathSources(),
                    "to-xembly.xsl"
                ).with("testing", "no")
            )
        ),
        SodgMojo.class
    );

    /**
     * SODG to Dot.
     */
    private static final Train<Shift> TO_DOT = new TrLogged(
        new TrFast(
            new TrClasspath<>(
                "/org/eolang/maven/sodg-to/normalize-attrs.xsl",
                "/org/eolang/maven/sodg-to/to-dot.xsl"
            ).back(),
            SodgMojo.class
        ),
        SodgMojo.class,
        Level.FINEST
    );

    /**
     * Graph modification right after it's generated from Xembly.
     */
    private static final Train<Shift> FINISH = new TrLogged(
        new TrFast(
            new TrClasspath<>(
                "/org/eolang/maven/sodg-to/catch-lost-edges.xsl",
                "/org/eolang/maven/sodg-to/catch-duplicate-edges.xsl",
                "/org/eolang/maven/sodg-to/catch-crowded-epsilons.xsl",
                "/org/eolang/maven/sodg-to/catch-crowded-betas.xsl",
                "/org/eolang/maven/sodg-to/catch-conflicting-greeks.xsl",
                "/org/eolang/maven/sodg-to/catch-empty-edges.xsl"
            ).back(),
            SodgMojo.class
        ),
        SodgMojo.class,
        Level.FINEST
    );

    /**
     * The train that generates SODG.
     */
    private static final Train<Shift> TRAIN = new TrWith(
        new TrFast(
            new TrJoined<>(
                new TrClasspath<>(
                    "/org/eolang/maven/sodg/pre-clean.xsl",
                    "/org/eolang/maven/sodg/remove-leveled.xsl"
                ).back(),
                new TrDefault<>(
                    new StEndless(
                        new StXPath(
                            "(//o[@name and @atom and not(@base) and @loc and not(@lambda)])[1]",
                            xml -> {
                                final String loc = xml.xpath("@loc").get(0);
                                return new Directives().attr(
                                    "lambda",
                                    SodgMojo.locToHex(
                                        loc.substring(loc.indexOf('.') + 1)
                                    )
                                );
                            }
                        )
                    )
                ),
                new TrLogged(
                    new TrMapped<>(
                        (Function<String, Shift>) path -> new StBefore(
                            new StClasspath(path),
                            new StClasspath(
                                "/org/eolang/maven/sodg/before-each.xsl",
                                String.format("sheet %s", path)
                            )
                        ),
                        "/org/eolang/maven/sodg/add-sodg-root.xsl",
                        "/org/eolang/maven/sodg/add-loc-to-objects.xsl",
                        "/org/eolang/maven/sodg/add-root.xsl",
                        "/org/eolang/maven/sodg/append-xi.xsl",
                        "/org/eolang/maven/sodg/touch-all.xsl",
                        "/org/eolang/maven/sodg/bind-rho-and-sigma.xsl",
                        "/org/eolang/maven/sodg/pi-copies.xsl",
                        "/org/eolang/maven/sodg/epsilon-bindings.xsl",
                        "/org/eolang/maven/sodg/connect-dots.xsl",
                        "/org/eolang/maven/sodg/put-data.xsl",
                        "/org/eolang/maven/sodg/put-atoms.xsl"
                    ).back(),
                    SodgMojo.class,
                    Level.FINEST
                ),
                new TrClasspath<>(
                    "/org/eolang/maven/sodg/focus.xsl",
                    "/org/eolang/maven/sodg/add-license.xsl"
                ).back()
            ),
            SodgMojo.class
        ),
        new StSchema("/org/eolang/maven/sodg/after.xsd")
    );

    /**
     * Shall we generate .xml files with SODGs?
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.generateSodgXmlFiles",
        defaultValue = "false"
    )
    @SuppressWarnings("PMD.LongVariable")
    private boolean generateSodgXmlFiles;

    /**
     * Shall we generate .xe files with Xembly instructions graph?
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.generateXemblyFiles",
        defaultValue = "false"
    )
    @SuppressWarnings("PMD.LongVariable")
    private boolean generateXemblyFiles;

    /**
     * Shall we generate .graph.xml files with XML graph?
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.generateGraphFiles",
        defaultValue = "false"
    )
    @SuppressWarnings("PMD.LongVariable")
    private boolean generateGraphFiles;

    /**
     * Shall we generate .dot files with DOT language graph commands?
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.generateDotFiles",
        defaultValue = "false"
    )
    @SuppressWarnings("PMD.LongVariable")
    private boolean generateDotFiles;

    /**
     * List of object names to participate in SODG generation.
     * @implNote {@code property} attribute is omitted for collection
     *  properties since there is no way of passing it via command line.
     * @checkstyle MemberNameCheck (15 lines)
     */
    @Parameter
    private Set<String> sodgIncludes = new SetOf<>("**");

    /**
     * List of object names which are excluded from SODG generation.
     * @implNote {@code property} attribute is omitted for collection
     *  properties since there is no way of passing it via command line.
     * @checkstyle MemberNameCheck (15 lines)
     */
    @Parameter
    private Set<String> sodgExcludes = new SetOf<>();

    @Override
    public void exec() throws IOException {
        if (this.generateGraphFiles && !this.generateXemblyFiles) {
            throw new IllegalStateException(
                "Setting generateGraphFiles and not setting generateXemblyFiles has no effect because .graph files require .xe files"
            );
        }
        if (this.generateDotFiles && !this.generateGraphFiles) {
            throw new IllegalStateException(
                "Setting generateDotFiles and not setting generateGraphFiles has no effect because .dot files require .graph files"
            );
        }
        final Collection<Tojo> tojos = this.scopedTojos().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR2)
        );
        final Path home = this.targetDir.toPath().resolve(SodgMojo.DIR);
        int total = 0;
        int instructions = 0;
        final Set<Pattern> includes = this.sodgIncludes.stream()
            .map(i -> Pattern.compile(SodgMojo.createMatcher(i)))
            .collect(Collectors.toSet());
        final Set<Pattern> excludes = this.sodgExcludes.stream()
            .map(i -> Pattern.compile(SodgMojo.createMatcher(i)))
            .collect(Collectors.toSet());
        for (final Tojo tojo : tojos) {
            final String name = tojo.get(Tojos.KEY);
            if (this.exclude(name, includes, excludes)) {
                continue;
            }
            final Path sodg = new Place(name).make(home, "sodg");
            final Path xmir = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2));
            if (sodg.toFile().lastModified() >= xmir.toFile().lastModified()) {
                Logger.debug(
                    this, "Already converted %s to %s (it's newer than the source)",
                    name, new Rel(sodg)
                );
                continue;
            }
            final int extra = this.render(xmir, sodg);
            instructions += extra;
            tojo.set(AssembleMojo.ATTR_SODG, sodg.toAbsolutePath().toString());
            Logger.info(
                this, "SODG for %s saved to %s (%d instructions)",
                name, new Rel(sodg), extra
            );
            ++total;
        }
        if (total == 0) {
            if (tojos.isEmpty()) {
                Logger.info(this, "No .xmir need to be converted to SODGs");
            } else {
                Logger.info(this, "No .xmir converted to SODGs");
            }
        } else {
            Logger.info(
                this, "Converted %d .xmir to SODGs, saved to %s, %d instructions",
                total, new Rel(home), instructions
            );
        }
    }

    /**
     * Creates a regular expression out of sodgInclude string.
     * @param pattern String from sodgIncludes
     * @return Created regular expression
     */
    private static String createMatcher(final String pattern) {
        return pattern
            .replace("**", "[A-Za-z0-9.]+?")
            .replace("*", "[A-Za-z0-9]+");
    }

    /**
     * Exclude this EO program from processing?
     * @param name The name
     * @param includes Patterns for sodgs to be included
     * @param excludes Patterns for sodgs to be excluded
     * @return TRUE if to exclude
     */
    private boolean exclude(
        final String name,
        final Set<Pattern> includes,
        final Set<Pattern> excludes
    ) {
        boolean exclude = false;
        if (includes.stream().noneMatch(p -> p.matcher(name).matches())) {
            Logger.debug(this, "Excluding %s due to sodgIncludes option", name);
            exclude = true;
        }
        if (excludes.stream().anyMatch(p -> p.matcher(name).matches())) {
            Logger.debug(this, "Excluding %s due to sodgExcludes option", name);
            exclude = true;
        }
        return exclude;
    }

    /**
     * Convert XMIR file to SODG.
     *
     * @param xmir Location of XMIR
     * @param sodg Location of SODG
     * @return Total number of SODG instructions generated
     * @throws IOException If fails
     */
    private int render(final Path xmir, final Path sodg) throws IOException {
        final XML before = new XMLDocument(xmir);
        if (Logger.isTraceEnabled(this)) {
            Logger.trace(this, "XML before translating to SODG:\n%s", before);
        }
        final XML after = new Xsline(SodgMojo.TRAIN).pass(before);
        final String instructions = new Xsline(SodgMojo.TO_TEXT)
            .pass(after)
            .xpath("/text/text()")
            .get(0);
        if (Logger.isTraceEnabled(this)) {
            Logger.trace(this, "SODGs:\n%s", instructions);
        }
        new Home(sodg.getParent()).save(instructions, sodg.getParent().relativize(sodg));
        if (this.generateSodgXmlFiles) {
            final Path sibling = sodg.resolveSibling(String.format("%s.xml", sodg.getFileName()));
            new Home(sibling.getParent()).save(
                after.toString(),
                sibling.getParent().relativize(sibling)
            );
        }
        if (this.generateXemblyFiles) {
            final String xembly = new Xsline(SodgMojo.TO_XEMBLY)
                .pass(after)
                .xpath("/xembly/text()").get(0);
            final Path sibling = sodg.resolveSibling(String.format("%s.xe", sodg.getFileName()));
            new Home(sibling.getParent()).save(
                xembly,
                sibling.getParent().relativize(sibling)
            );
            this.makeGraph(xembly, sodg);
        }
        return instructions.split("\n").length;
    }

    /**
     * Make graph.
     * @param xembly The Xembly script
     * @param sodg The path of SODG file
     * @throws IOException If fails
     */
    private void makeGraph(final String xembly, final Path sodg) throws IOException {
        if (this.generateGraphFiles) {
            final Directives all = new Directives(xembly);
            Logger.debug(
                this, "There are %d Xembly directives for %s",
                new IoChecked<>(new LengthOf(all)).value(), sodg
            );
            final ListOf<Directive> directives = new ListOf<>(all);
            final Directive comment = directives.remove(0);
            final XML graph = new Xsline(SodgMojo.FINISH).pass(
                new XMLDocument(
                    new Xembler(
                        new Directives()
                            .append(Collections.singleton(comment))
                            .append(directives)
                    ).domQuietly()
                )
            );
            final Path sibling = sodg.resolveSibling(
                String.format("%s.graph.xml", sodg.getFileName())
            );
            new Home(sibling.getParent()).save(
                graph.toString(),
                sibling.getParent().relativize(sibling)
            );
            if (Logger.isTraceEnabled(this)) {
                Logger.trace(this, "Graph:\n%s", graph.toString());
            }
            this.makeDot(graph, sodg);
        }
    }

    /**
     * Make graph.
     * @param graph The graph in XML
     * @param sodg The path of SODG file
     * @throws IOException If fails
     */
    private void makeDot(final XML graph, final Path sodg) throws IOException {
        if (this.generateDotFiles) {
            final String dot = new Xsline(SodgMojo.TO_DOT)
                .pass(graph).xpath("//dot/text()").get(0);
            if (Logger.isTraceEnabled(this)) {
                Logger.trace(this, "Dot:\n%s", dot);
            }
            final Path sibling = sodg.resolveSibling(String.format("%s.dot", sodg.getFileName()));
            new Home(sibling.getParent()).save(
                dot,
                sibling.getParent().relativize(sibling)
            );
        }
    }

    /**
     * Lambda to HEX.
     * @param loc The lambda
     * @return Hexadecimal value as string.
     */
    private static String locToHex(final String loc) {
        final StringJoiner out = new StringJoiner("-");
        for (final byte bty : loc.getBytes(StandardCharsets.UTF_8)) {
            out.add(String.format("%02X", bty));
        }
        return out.toString();
    }

}
