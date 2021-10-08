/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.eolang.parser.Syntax;
import org.eolang.tojos.Tojo;

/**
 * Parse EO to XML.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
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

    @Override
    public void exec() throws IOException {
        final Collection<Tojo> tojos = this.tojos().select(
            row -> row.exists(AssembleMojo.ATTR_EO)
        );
        for (final Tojo tojo : tojos) {
            if (tojo.exists(AssembleMojo.ATTR_XMIR)) {
                Logger.debug(
                    this, "Already parsed %s to %s",
                    tojo.get("id"), Save.rel(Paths.get(tojo.get(AssembleMojo.ATTR_XMIR)))
                );
                continue;
            }
            this.parse(tojo);
        }
    }

    /**
     * Parse EO file to XML.
     *
     * @param tojo The tojo
     * @throws IOException If fails
     */
    private void parse(final Tojo tojo) throws IOException {
        final Path source = Paths.get(tojo.get(AssembleMojo.ATTR_EO));
        final String name = tojo.get("id");
        final Path target = new Place(name).make(
            this.targetDir.toPath().resolve(ParseMojo.DIR), "eo.xml"
        );
        if (Files.exists(target)) {
            Logger.info(
                this, "Already parsed %s to %s",
                Save.rel(source), Save.rel(target)
            );
        } else {
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            new Syntax(
                name,
                new InputOf(source),
                new OutputTo(baos)
            ).parse();
            new Save(baos.toByteArray(), target).save();
            Logger.info(
                this, "Parsed %s to %s",
                Save.rel(source), Save.rel(target)
            );
        }
        tojo.set(AssembleMojo.ATTR_XMIR, target.toAbsolutePath().toString());
    }

}
