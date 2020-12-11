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
import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.ResourceOf;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;
import org.cactoos.text.FormattedText;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Compile.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "compile",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class CompileMojo extends AbstractMojo {

    /**
     * Maven project.
     */
    @Parameter(defaultValue = "${project}")
    private MavenProject project;

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/generated-sources/eo"
    )
    private transient File generatedDir;

    /**
     * Target directory.
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
        if (this.generatedDir.mkdirs()) {
            Logger.info(this, "Gen directory created: %s", this.generatedDir);
        }
        final Path dir = this.targetDir.toPath().resolve("eo/optimize");
        try {
            Files.walk(dir)
                .filter(file -> !file.toFile().isDirectory())
                .forEach(this::compile);
        } catch (final IOException ex) {
            throw new MojoFailureException(
                new UncheckedText(
                    new FormattedText(
                        "Can't list EO files in %s",
                        dir
                    )
                ).asString(),
                ex
            );
        }
        this.project.addCompileSourceRoot(
            this.generatedDir.getAbsolutePath()
        );
        Logger.info(
            this, "Directory added to sources: %s",
            this.generatedDir
        );
    }

    /**
     * Compile one XML file.
     *
     * @param file XML file
     */
    private void compile(final Path file) {
        final Path temp = this.targetDir.toPath().resolve("eo/compile");
        try {
            final XML xml = new XMLDocument(file);
            final String name = xml.xpath("/program/@name").get(0);
            final XML out = new XSLDocument(
                new TextOf(
                    new ResourceOf("org/eolang/maven/to-java.xsl")
                ).asString()
            ).with(new ClasspathSources()).transform(xml);
            new IoChecked<>(
                new LengthOf(
                    new TeeInput(
                        new InputOf(out.toString()),
                        new OutputTo(
                            temp.resolve(String.format("%s.xml", name))
                        )
                    )
                )
            ).value();
            for (final XML java : out.nodes("/program/objects/o[java]")) {
                CompileMojo.save(
                    this.generatedDir.toPath().resolve(
                        Paths.get(
                            String.format(
                                "%s.java", java.xpath("@java-name").get(0)
                            )
                        )
                    ),
                    java.xpath("java/text()").get(0)
                );
            }
        } catch (final IOException ex) {
            throw new IllegalStateException(
                new UncheckedText(
                    new FormattedText(
                        "Can't compile %s into %s",
                        file, this.generatedDir
                    )
                ).asString(),
                ex
            );
        }
        Logger.info(this, "%s compiled to %s", file, this.generatedDir);
    }

    /**
     * Save one Java file.
     * @param path The path
     * @param content The content
     * @throws IOException If fails
     */
    private static void save(final Path path, final String content)
        throws IOException {
        new IoChecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(content),
                    path
                )
            )
        ).value();
        Logger.info(
            CompileMojo.class,
            "Saved %d chars to %s",
            content.length(),
            path.toAbsolutePath()
        );
    }

}
