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
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
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
     * Whether we should fail on error.
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
        final Collection<Tojo> tojos = this.scopedTojos().select(
            row -> row.exists(AssembleMojo.ATTR_EO)
        );
        int total = 0;
        for (final Tojo tojo : tojos) {
            if (tojo.exists(AssembleMojo.ATTR_XMIR)) {
                final Path xmir = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR));
                final Path src = Paths.get(tojo.get(AssembleMojo.ATTR_EO));
                if (xmir.toFile().lastModified() >= src.toFile().lastModified()) {
                    Logger.debug(
                        this, "Already parsed %s to %s (it's newer than the source)",
                        tojo.get(Tojos.KEY), Save.rel(xmir)
                    );
                    continue;
                }
            }
            this.parse(tojo);
            ++total;
        }
        if (total == 0) {
            if (tojos.isEmpty()) {
                Logger.info(this, "No .eo sources need to be parsed to XMIRs");
            } else {
                Logger.info(this, "No .eo sources parsed to XMIRs");
            }
        } else {
            Logger.info(this, "Parsed %d .eo sources to XMIRs", total);
        }
    }

    /**
     * Parse EO file to XML.
     *
     * @param tojo The tojo
     * @throws IOException If fails
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    private void parse(final Tojo tojo) throws IOException {
        final Path source = Paths.get(tojo.get(AssembleMojo.ATTR_EO));
        final String name = tojo.get(Tojos.KEY);
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            new Syntax(
                name,
                new InputOf(source),
                new OutputTo(baos)
            ).parse();
        // @checkstyle IllegalCatchCheck (1 line)
        } catch (final RuntimeException ex) {
            if (this.failOnError) {
                throw new IllegalArgumentException(
                    String.format("Failed to parse %s", source),
                    ex
                );
            }
            Logger.warn(
                this, "Parse was skipped due to failOnError=false. In file %s with error: %s",
                source.toString(),
                ex.getMessage()
            );
            return;
        }
        final Path target = new Place(name).make(
            this.targetDir.toPath().resolve(ParseMojo.DIR), Transpiler.EXT
        );
        new Save(
            new XMLDocument(
                new Xembler(
                    new Directives().xpath("/program").attr(
                        "source", source.toAbsolutePath()
                    )
                ).applyQuietly(new XMLDocument(baos.toByteArray()).node())
            ).toString(),
            target
        ).save();
        Logger.debug(
            this, "Parsed %s to %s",
            Save.rel(source), Save.rel(target)
        );
        tojo.set(AssembleMojo.ATTR_XMIR, target.toAbsolutePath().toString());
    }

}
