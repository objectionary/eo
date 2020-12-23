/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
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
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Pull EO XML files.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "pull",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class PullMojo extends AbstractMojo {

    /**
     * From directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}"
    )
    private transient File targetDir;

    @Override
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        final Path dir = this.targetDir.toPath().resolve("eo/optimize");
        try {
            Files.walk(dir)
                .filter(file -> !file.toFile().isDirectory())
                .forEach(this::pull);
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
     * Pull all deps found in XML file.
     *
     * @param file EO file
     */
    private void pull(final Path file) {
        try {
            final String xpath = String.join(
                "",
                "//o[@base and contains(@base, '.') ",
                "and not(starts-with(@base, '.'))]/@base"
            );
            for (final String name : new XMLDocument(file).xpath(xpath)) {
                this.pull(name);
            }
        } catch (final IOException ex) {
            throw new IllegalStateException(
                new UncheckedText(
                    new FormattedText(
                        "Can't pull %s into %s",
                        file, this.targetDir
                    )
                ).asString(),
                ex
            );
        }
    }

    /**
     * Pull one dep.
     *
     * @param name Name of the object, e.g. "org.eolang.io.stdout"
     * @throws IOException If fails
     */
    private void pull(final String name) throws IOException {
        final String res = String.format("/%s.eo.xml", name.replace(".", "/"));
        final InputStream input = this.getClass().getResourceAsStream(res);
        if (input == null) {
            throw new IllegalStateException(
                String.format(
                    "Can't find \"%s\" in classpath",
                    res
                )
            );
        }
        final Path path = this.targetDir.toPath()
            .resolve("eo/pull")
            .resolve(String.format("%s.eo.xml", name.replace(".", "/")));
        new IoChecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(input),
                    new OutputTo(path)
                )
            )
        ).value();
        Logger.info(this, "%s pulled to %s", name, path);
    }

}
