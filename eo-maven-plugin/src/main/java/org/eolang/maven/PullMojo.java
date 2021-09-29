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
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Func;
import org.cactoos.Input;
import org.cactoos.func.IoCheckedFunc;
import org.cactoos.io.InputOf;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Pull EO XML files from Objectionary.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "pull",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class PullMojo extends AbstractMojo {

    /**
     * Where we have .eo.xml files just parsed (we pull new .eo.xml
     * files right here too).
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo"
    )
    private File targetDir;

    /**
     * The repo.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Func<String, Input> repo = name -> new InputOf(
        new URL(
            String.format(
                "https://www.objectionary.com/xml/%s.eo.xml",
                name.replace(".", "/")
            )
        )
    );

    @Override
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        final Path dir = this.targetDir.toPath().resolve("01-parse");
        try {
            final List<Path> files = Files.walk(dir)
                .filter(file -> !file.toFile().isDirectory())
                .collect(Collectors.toList());
            Logger.info(this, "%d eo.xml files found", files.size());
            files.forEach(file -> this.pull(dir, file));
        } catch (final IOException ex) {
            throw new MojoFailureException(
                String.format(
                    "Can't list XML files in %s",
                    dir
                ),
                ex
            );
        }
    }

    /**
     * Pull all deps found in XML file.
     *
     * @param dir The dir
     * @param file EO file
     */
    private void pull(final Path dir, final Path file) {
        try {
            final List<String> names = new XMLDocument(file).xpath(
                "//meta[head='alias']/part[2]/text()"
            );
            for (final String name : names) {
                this.pull(dir, name);
            }
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "Can't pull %s into %s",
                    file, this.targetDir
                ),
                ex
            );
        }
    }

    /**
     * Pull one dep.
     *
     * @param dir The dir
     * @param name Name of the object, e.g. "org.eolang.io.stdout"
     * @throws IOException If fails
     */
    private void pull(final Path dir, final String name) throws IOException {
        final Path path = dir.resolve(
            String.format("%s.eo.xml", name.replace(".", "/"))
        );
        if (path.toFile().exists()) {
            Logger.info(this, "The file %s already exists", path);
        } else {
            new Save(
                new IoCheckedFunc<>(this.repo).apply(name),
                path
            ).save();
            Logger.info(this, "Object %s pulled to %s", name, path);
        }
    }

}
