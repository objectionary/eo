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
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.footprint.Footprint;
import org.eolang.maven.footprint.FtCached;
import org.eolang.maven.footprint.FtDefault;
import org.eolang.maven.util.Rel;
import org.eolang.parser.ParsingException;
import org.eolang.parser.Syntax;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Parse EO to XML.
 *
 * @since 0.1
 */
@Mojo(
    name = "parse",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.ImmutableField")
public final class ParseMojo extends SafeMojo {

    /**
     * Zero version.
     */
    public static final String ZERO = "0.0.0";

    /**
     * The directory where to parse to.
     */
    public static final String DIR = "01-parse";

    /**
     * Subdirectory for parsed cache.
     */
    public static final String PARSED = "parsed";

    /**
     * EO cache directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.cache")
    @SuppressWarnings("PMD.ImmutableField")
    private Path cache = Paths.get(System.getProperty("user.home")).resolve(".eo");

    /**
     * Whether we should fail on parsing error.
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.23.0
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(
        property = "eo.failOnError",
        defaultValue = "true")
    private boolean failOnError = true;

    @Override
    public void exec() throws IOException {
        final int total = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    tojo -> (Scalar<Integer>) () -> {
                        this.parse(tojo);
                        return 1;
                    },
                    new Filtered<>(
                        this::isNotParsed,
                        this.scopedTojos().select(row -> row.exists(AssembleMojo.ATTR_EO))
                    )
                )
            )
        ).intValue();
        if (0 == total) {
            if (((Collection<Tojo>) this.scopedTojos().select(
                row -> row.exists(AssembleMojo.ATTR_EO)
            )).isEmpty()) {
                Logger.info(this, "No .eo sources need to be parsed to XMIRs");
            } else {
                Logger.info(this, "No .eo sources parsed to XMIRs");
            }
        } else {
            Logger.info(this, "Parsed %d .eo sources to XMIRs", total);
        }
    }

    /**
     * Check if the given tojo has not been parsed.
     *
     * @param tojo Tojo.
     * @return True if the tojo has not been parsed.
     */
    private boolean isNotParsed(final Tojo tojo) {
        boolean res = true;
        if (tojo.exists(AssembleMojo.ATTR_XMIR)) {
            final Path xmir = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR));
            final Path src = Paths.get(tojo.get(AssembleMojo.ATTR_EO));
            if (xmir.toFile().lastModified() >= src.toFile().lastModified()) {
                Logger.debug(
                    this, "Already parsed %s to %s (it's newer than the source)",
                    tojo.get(Tojos.KEY), new Rel(xmir)
                );
                res = false;
            }
        }
        return res;
    }

    /**
     * Parse EO file to XML.
     *
     * @param tojo The tojo
     * @throws IOException If fails
     */
    @SuppressWarnings({"PMD.AvoidCatchingGenericException", "PMD.ExceptionAsFlowControl"})
    private void parse(final Tojo tojo) throws IOException {
        final Path source = Paths.get(tojo.get(AssembleMojo.ATTR_EO));
        final String name = tojo.get(Tojos.KEY);
        Footprint footprint;
        footprint = new FtDefault(
            this.targetDir.toPath().resolve(ParseMojo.DIR)
        );
        if (tojo.exists(AssembleMojo.ATTR_HASH)) {
            footprint = new FtCached(
                tojo.get(AssembleMojo.ATTR_HASH),
                this.cache.resolve(ParseMojo.PARSED),
                footprint
            );
        }
        try {
            footprint.save(
                name,
                AssembleMojo.ATTR_XMIR,
                () -> {
                    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                    new Syntax(
                        name,
                        new InputOf(source),
                        new OutputTo(baos)
                    ).parse();
                    final XMLDocument parsed = new ParseMojo.WithValidVersion(
                        new XMLDocument(
                            new Xembler(
                                new Directives().xpath("/program").attr(
                                    "source",
                                    source.toAbsolutePath()
                                )
                            ).applyQuietly(new XMLDocument(baos.toByteArray()).node())
                        )
                    ).value();
                    Logger.debug(
                        this,
                        "Parsed program %s:\n %s",
                        name,
                        parsed.toString()
                    );
                    return parsed.toString();
                }
            );
        } catch (final ParsingException ex) {
            if (this.failOnError) {
                throw new IllegalArgumentException(
                    String.format("Failed to parse %s", source),
                    ex
                );
            }
            Logger.warn(
                this, "Parsing was skipped due to failOnError=false. In file %s with error: %s",
                source,
                ex.getMessage()
            );
            return;
        }
        final Path target = new Place(name).make(
            this.targetDir.toPath().resolve(ParseMojo.DIR),
            TranspileMojo.EXT
        );
        tojo.set(AssembleMojo.ATTR_XMIR, target.toAbsolutePath().toString());
        Logger.debug(
            this, "Parsed %s to %s",
            new Rel(source), new Rel(target)
        );
    }

    /**
     * Class that validates version meta within XMIR.
     *
     * @since 1.0
     */
    static final class WithValidVersion implements Scalar<XMLDocument> {

        /**
         * Source XMIR.
         */
        private final XMLDocument xmir;

        /**
         * Ctor.
         * @param xmir Source
         */
        WithValidVersion(final XMLDocument xmir) {
            this.xmir = xmir;
        }

        @Override
        public XMLDocument value() throws Exception {
            final List<String> ver =
                this.xmir.xpath("/program/metas/meta[./head='version']/tail/text()");
            final List<String> path = this.xmir.xpath("/program/@source");
            if (!this.xmir.xpath("/program/metas/meta[./head='version']/tail/text()").isEmpty()
                && !this.xmir.xpath("/program/@source").isEmpty()
                && Pattern.compile("src/(?:main|test)").matcher(path.get(0)).find()
                && !ver.get(0).equals("0.0.0")
            ) {
                throw new ParsingException(
                    String.format(
                        String.join(
                            "",
                            "Incorrect version '%s' found in %s: ",
                            "+version meta must be '0.0.0' during development."
                        ),
                        ver.get(0),
                        path.get(0)
                    ),
                    new IllegalArgumentException(),
                    Integer.parseInt(
                        this.xmir.xpath("/program/metas/meta[./head='version']/@line").get(0)
                    )
                );
            }
            return this.xmir;
        }
    }
}
