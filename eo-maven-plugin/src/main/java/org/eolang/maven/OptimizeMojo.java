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
import com.jcabi.xml.XMLDocument;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.io.OutputTo;
import org.cactoos.list.ListOf;
import org.eolang.parser.Xsline;
import org.eolang.tojos.Tojo;

/**
 * Optimize XML files.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "optimize",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class OptimizeMojo extends SafeMojo {

    /**
     * The directory where to place intermediary files.
     */
    public static final String STEPS = "02-steps";

    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "03-optimize";

    @Override
    public void exec() throws IOException {
        final Collection<Tojo> sources = this.tojos().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR)
                && !row.exists(AssembleMojo.ATTR_XMIR2)
        );
        for (final Tojo source : sources) {
            source.set(
                AssembleMojo.ATTR_XMIR2,
                this.optimize(
                    Paths.get(source.get(AssembleMojo.ATTR_XMIR))
                ).toAbsolutePath().toString()
            );
        }
        Logger.info(this, "%d XMIR programs optimized", sources.size());
    }

    /**
     * Optimize XML file after parsing.
     *
     * @param file EO file
     * @return The file with optimized XMIR
     * @throws IOException If fails
     */
    private Path optimize(final Path file) throws IOException {
        final Place place = new Place(
            new XMLDocument(file).xpath("/program/@name").get(0)
        );
        final Path dir = place.make(
            this.targetDir.toPath().resolve(OptimizeMojo.STEPS), ""
        );
        final Path target = place.make(
            this.targetDir.toPath().resolve(OptimizeMojo.DIR), "eo.xml"
        );
        if (Files.exists(target)) {
            Logger.info(
                this, "%s already optimized to %s, all steps are in %s",
                file, target, dir
            );
        } else {
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            new Xsline(
                new XMLDocument(file),
                new OutputTo(baos),
                new TargetSpy(dir)
            ).with(
                new ListOf<>(
                    "org/eolang/parser/optimize/globals-to-abstracts.xsl",
                    "org/eolang/parser/optimize/remove-refs.xsl",
                    "org/eolang/parser/optimize/abstracts-float-up.xsl",
                    "org/eolang/parser/optimize/remove-levels.xsl",
                    "org/eolang/parser/add-refs.xsl",
                    "org/eolang/parser/optimize/fix-missed-names.xsl",
                    "org/eolang/parser/errors/broken-refs.xsl"
                )
            ).pass();
            new Save(baos.toByteArray(), target).save();
            Logger.info(
                this, "%s optimized to %s, all steps are in %s",
                file, target, dir
            );
            Logger.debug(
                this, "Optimized XML saved to %s:\n%s", target,
                new String(baos.toByteArray(), StandardCharsets.UTF_8)
            );
        }
        return target;
    }

}
