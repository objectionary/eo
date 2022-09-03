/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.StSchema;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrLogged;
import com.yegor256.xsline.TrWith;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Convert XMIR to GMI.
 *
 * @since 0.27
 */
@Mojo(
    name = "gmi",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.ImmutableField")
public final class GmiMojo extends SafeMojo {

    /**
     * The directory where to save GMI to.
     */
    public static final String DIR = "gmi";

    /**
     * GMI to text.
     */
    private static final Train<Shift> TO_TEXT = new TrLogged(
        new TrClasspath<>(
            new TrDefault<>(),
            "/org/eolang/maven/gmi-to/gmi-to-text.xsl"
        ).back(),
        GmiMojo.class
    );

    /**
     * GMI to Xembly.
     */
    private static final Train<Shift> TO_XEMBLY = new TrLogged(
        new TrClasspath<>(
            new TrDefault<>(),
            "/org/eolang/maven/gmi-to/gmi-to-xembly.xsl"
        ).back(),
        GmiMojo.class
    );

    /**
     * Xembly to Dot.
     */
    private static final Train<Shift> TO_DOT = new TrLogged(
        new TrClasspath<>(
            new TrDefault<>(),
            "/org/eolang/maven/gmi-to/verify-edges.xsl",
            "/org/eolang/maven/gmi-to/to-dot.xsl"
        ).back(),
        GmiMojo.class
    );

    /**
     * The train that generates GMI.
     */
    private static final Train<Shift> TRAIN = new TrWith(
        new TrLogged(
            new TrClasspath<>(
                new TrDefault<>(),
                "/org/eolang/maven/gmi/R0.xsl",
                "/org/eolang/maven/gmi/R1.xsl",
                "/org/eolang/maven/gmi/R1.1.xsl",
                "/org/eolang/maven/gmi/R4.xsl",
                "/org/eolang/maven/gmi/R5.xsl",
                "/org/eolang/maven/gmi/R6.xsl",
                "/org/eolang/maven/gmi/R7.xsl",
                "/org/eolang/maven/gmi/focus.xsl",
                "/org/eolang/maven/gmi/rename.xsl",
                "/org/eolang/maven/gmi/strip.xsl",
                "/org/eolang/maven/gmi/variability.xsl"
            ).back(),
            GmiMojo.class
        ),
        new StLambda(
            "escape-data",
            xml -> {
                final Node dom = xml.node();
                GmiMojo.escape(dom);
                return new XMLDocument(dom);
            }
        ),
        new StSchema("/org/eolang/maven/gmi/after.xsd")
    );

    /**
     * Shall we generate .xml files with GMIs?
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.generateGmiXmlFiles",
        defaultValue = "false"
    )
    @SuppressWarnings("PMD.LongVariable")
    private boolean generateGmiXmlFiles;

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
     * Shall we generate .graph files with XML graph?
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
        final Path home = this.targetDir.toPath().resolve(GmiMojo.DIR);
        int total = 0;
        int instructions = 0;
        for (final Tojo tojo : tojos) {
            final Path gmi = new Place(tojo.get(Tojos.KEY)).make(home, "gmi");
            final Path xmir = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2));
            if (gmi.toFile().lastModified() >= xmir.toFile().lastModified()) {
                Logger.debug(
                    this, "Already converted %s to %s (it's newer than the source)",
                    tojo.get(Tojos.KEY), Save.rel(gmi)
                );
                continue;
            }
            instructions += this.render(xmir, gmi);
            tojo.set(AssembleMojo.ATTR_GMI, gmi.toAbsolutePath().toString());
            ++total;
        }
        if (total == 0) {
            if (tojos.isEmpty()) {
                Logger.info(this, "No .xmir need to be converted to GMIs");
            } else {
                Logger.info(this, "No .xmir converted to GMIs");
            }
        } else {
            Logger.info(
                this, "Converted %d .xmir to GMIs, saved to %s, %d instructions",
                total, Save.rel(home), instructions
            );
        }
    }

    /**
     * Convert XMIR file to GMI.
     *
     * @param xmir Location of XMIR
     * @param gmi Location of GMI
     * @return Total number of GMI instructions generated
     * @throws IOException If fails
     */
    private int render(final Path xmir, final Path gmi) throws IOException {
        final XML before = new XMLDocument(xmir);
        Logger.debug(this, "XML before translating to GMI:\n%s", before);
        final XML after = new Xsline(GmiMojo.TRAIN).pass(before);
        final String instructions = new Xsline(GmiMojo.TO_TEXT)
            .pass(after)
            .xpath("/text/text()")
            .get(0);
        new Save(instructions, gmi).save();
        if (this.generateGmiXmlFiles) {
            new Save(
                after.toString(),
                gmi.resolveSibling(String.format("%s.xml", gmi.getFileName()))
            ).save();
        }
        if (this.generateXemblyFiles) {
            final String xembly = new Xsline(GmiMojo.TO_XEMBLY)
                .pass(after)
                .xpath("/xembly/text()").get(0);
            new Save(
                xembly,
                gmi.resolveSibling(String.format("%s.xe", gmi.getFileName()))
            ).save();
            this.makeGraph(xembly, gmi);
        }
        final int total = instructions.split("\n").length;
        Logger.info(
            this, "GMI for %s saved to %s (%d instructions)",
            Save.rel(xmir), Save.rel(gmi), total
        );
        return total;
    }

    /**
     * Make graph.
     * @param xembly The Xembly script
     * @param gmi The path of GMI file
     * @throws IOException If fails
     */
    private void makeGraph(final String xembly, final Path gmi) throws IOException {
        if (this.generateGraphFiles) {
            final XML graph = new XMLDocument(
                new Xembler(
                    new Directives()
                        .add("test")
                        .add("graph")
                        .add("v")
                        .attr("id", "ν0")
                        .append(new Directives(xembly))
                ).domQuietly()
            );
            new Save(
                graph.toString(),
                gmi.resolveSibling(String.format("%s.graph", gmi.getFileName()))
            ).save();
            if (this.generateDotFiles) {
                new Save(
                    new Xsline(GmiMojo.TO_DOT).pass(graph).xpath("//dot/text()").get(0),
                    gmi.resolveSibling(String.format("%s.dot", gmi.getFileName()))
                ).save();
            }
        }
    }

    /**
     * Escape all texts in all "a" elements.
     * @param node The node
     */
    private static void escape(final Node node) {
        if ("a".equals(node.getLocalName())
            && "data".equals(node.getAttributes().getNamedItem("prefix").getTextContent())) {
            final String text = node.getTextContent();
            final StringBuilder out = new StringBuilder(text.length());
            for (final char chr : text.toCharArray()) {
                if (chr >= ' ' && chr <= '}' && chr != '\'' && chr != '"') {
                    out.append(chr);
                } else {
                    out.append("\\u").append(String.format("%04x", (int) chr));
                }
            }
            node.setTextContent(out.toString());
        }
        if (node.hasChildNodes()) {
            final NodeList kids = node.getChildNodes();
            for (int idx = 0; idx < kids.getLength(); ++idx) {
                GmiMojo.escape(kids.item(idx));
            }
        }
    }

}
