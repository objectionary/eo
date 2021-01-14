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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;
import org.cactoos.text.FormattedText;
import org.cactoos.text.UncheckedText;
import org.eolang.parser.Pack;
import org.eolang.parser.Program;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Optimize XML files.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "optimize",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class OptimizeMojo extends AbstractMojo {

    /**
     * From directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}"
    )
    private File targetDir;

    @Override
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        final Path dir = this.targetDir.toPath().resolve("eo/parse");
        try {
            Files.walk(dir)
                .filter(file -> !file.toFile().isDirectory())
                .forEach(file -> this.optimize(dir, file));
        } catch (final IOException ex) {
            throw new MojoFailureException(
                new UncheckedText(
                    new FormattedText(
                        "Can't list XML files in %s",
                        dir
                    )
                ).asString(),
                ex
            );
        }
    }

    /**
     * Optimize XML file after parsing.
     *
     * @param home Where it was found
     * @param file EO file
     */
    private void optimize(final Path home, final Path file) {
        final String name = file.toString().substring(
            home.toString().length() + 1
        );
        final Path dir = this.targetDir.toPath()
            .resolve("eo/steps")
            .resolve(name);
        try {
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            new Program(new XMLDocument(file), new OutputTo(baos)).compile(
                new Pack()
                    .with("globals-to-abstracts.xsl")
                    .with("remove-refs.xsl")
                    .with("abstracts-float-up.xsl")
                    .with("remove-levels.xsl")
                    .with("add-refs.xsl")
                    .with("errors/broken-refs.xsl"),
                new TargetSpy(dir)
            );
            final Path target = this.targetDir.toPath()
                .resolve("eo/optimize")
                .resolve(name);
            new IoChecked<>(
                new LengthOf(
                    new TeeInput(
                        new InputOf(baos.toString()),
                        new OutputTo(target)
                    )
                )
            ).value();
            Logger.info(
                this, "%s optimized to %s, all steps are in %s",
                file, target, dir
            );
            Logger.debug(
                this, "Optimized XML saved to %s:\n%s",
                target, baos.toString()
            );
            final List<XML> errors = new XMLDocument(baos.toString())
                .nodes("/program/errors/error");
            for (final XML error : errors) {
                Logger.error(
                    this,
                    "[%s:%s] %s (%s:%s)",
                    name,
                    error.xpath("@line").get(0),
                    error.xpath("text()").get(0),
                    error.xpath("@check").get(0),
                    error.xpath("@step").get(0)
                );
            }
            if (!errors.isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "There are %d errors in %s, see log above",
                        errors.size(), file
                    )
                );
            }
        } catch (final IOException ex) {
            throw new IllegalStateException(
                new UncheckedText(
                    new FormattedText(
                        "Can't compile %s into %s",
                        file, this.targetDir
                    )
                ).asString(),
                ex
            );
        }
    }

}
