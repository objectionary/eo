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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Func;
import org.cactoos.Input;
import org.cactoos.func.IoCheckedFunc;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.list.ListOf;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Pull EO XML files from Objectionary and parse them into XML.
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
     * The directory where to process to.
     */
    public static final String DIR = "04-pull";

    /**
     * Where we have .eo.xml files just parsed (we process new .eo
     * files right here too, and parse them here too).
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
                // @checkstyle LineLength (1 line)
                "https://raw.githubusercontent.com/yegor256/objectionary/master/objects/%s.eo",
                name.replace(".", "/")
            )
        )
    );

    @Override
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        final Path sources = this.targetDir.toPath().resolve(OptimizeMojo.DIR);
        try {
            final List<Path> files = new Walk(sources);
            Logger.info(this, "%d eo.xml files found", files.size());
            final Collection<String> foreign = new HashSet<>(0);
            for (final Path xml : files) {
                foreign.addAll(this.process(xml));
            }
            Logger.info(this, "%d foreign objects found", foreign.size());
            for (final String name : foreign) {
                this.process(name);
            }
        } catch (final IOException ex) {
            throw new MojoFailureException(
                String.format(
                    "Can't list XML files in %s",
                    sources
                ),
                ex
            );
        }
    }

    /**
     * Pull all deps found in the provided XML file.
     *
     * @param file The .eo.xml file
     * @throws FileNotFoundException If not found
     */
    private Collection<String> process(final Path file)
        throws FileNotFoundException {
        final Collection<String> foreign = new HashSet<>(
            new ListOf<>(
                new Filtered<>(
                    obj -> !obj.isEmpty(),
                    new XMLDocument(file).xpath(
                        "//o[not(starts-with(@base,'.')) and not(@ref)]/@base"
                    )
                )
            )
        );
        if (foreign.isEmpty()) {
            Logger.info(
                this, "Didn't find any foreign objects in %s",
                file
            );
        } else {
            Logger.info(
                this, "Found %d foreign objects in %s: %s",
                foreign.size(), file, foreign
            );
        }
        return foreign;
    }

    /**
     * Pull one object by name.
     *
     * @param name The name of the object
     * @throws IOException If fails
     */
    private void process(final String name) throws IOException {
        final Path xml = new Place(name).make(
            this.targetDir.toPath().resolve(ParseMojo.DIR), "eo.xml"
        );
        if (xml.toFile().exists()) {
            Logger.debug(this, "The object %s already parsed at %s", name, xml);
        } else {
            new Parsing(this.pull(name)).into(
                this.targetDir, name
            );
        }
    }

    /**
     * Pull one object.
     *
     * @param name Name of the object, e.g. "org.eolang.io.stdout"
     * @return The path of .eo file
     * @throws IOException If fails
     */
    private Path pull(final String name) throws IOException {
        final Path src = new Place(name).make(
            this.targetDir.toPath().resolve(PullMojo.DIR), "eo"
        );
        if (src.toFile().exists()) {
            Logger.debug(this, "The object %s already pulled to %s", name, src);
        } else {
            new Save(
                new IoCheckedFunc<>(this.repo).apply(name),
                src
            ).save();
            Logger.info(this, "Sources of object %s pulled to %s", name, src);
        }
        return src;
    }

}
