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
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.cactoos.io.OutputTo;
import org.cactoos.list.ListOf;
import org.cactoos.text.FormattedText;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.transpiler.medium2target.Medium2TargetTranspiler;
import org.eolang.maven.transpiler.mediumcodemodel.EOSourceEntity;
import org.eolang.maven.transpiler.mediumcodemodel.EOSourceFile;
import org.eolang.maven.transpiler.mediumcodemodel.EOTargetFile;
import org.eolang.maven.transpiler.xml2medium.XML2MediumParser;
import org.eolang.parser.Xsline;
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
@SuppressWarnings("PMD.LongVariable")
public final class CompileMojo extends AbstractMojo {

    public static final String COMPILER_ORIGINAL = "original";
    public static final String COMPILER_HSE = "hse";

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
        defaultValue = "${project.build.directory}/generated-sources"
    )
    private File generatedDir;

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo"
    )
    private File targetDir;

    /**
     * Add to source root.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    @SuppressWarnings("PMD.ImmutableField")
    private boolean addSourcesRoot = true;

    /**
     * Add to test source root.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private boolean addTestSourcesRoot;

    @Parameter(
            property = "compiler",
            defaultValue = COMPILER_ORIGINAL
    )
    private String compiler = COMPILER_ORIGINAL;

    @Override
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        if (this.generatedDir.mkdirs()) {
            Logger.info(this, "Gen directory created: %s", this.generatedDir);
        }
        final Path dir = this.targetDir.toPath().resolve("03-optimize");
        try {
            switch (compiler){
                case COMPILER_HSE:
                    Files.walk(dir)
                            .filter(file -> !file.toFile().isDirectory())
                            .forEach(this::compileHSE);
                    break;
                case COMPILER_ORIGINAL:
                default:
                    Files.walk(dir)
                            .filter(file -> !file.toFile().isDirectory())
                            .forEach(this::compile);
            }

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
        if (this.addSourcesRoot) {
            this.project.addCompileSourceRoot(
                this.generatedDir.getAbsolutePath()
            );
        }
        if (this.addTestSourcesRoot) {
            this.project.addTestCompileSourceRoot(
                this.generatedDir.getAbsolutePath()
            );
        }
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
        final Path temp = this.targetDir.toPath().resolve("05-compile");
        final Path pre = this.targetDir.toPath().resolve("04-pre");
        try {
            final XML input = new XMLDocument(file);
            final String name = input.xpath("/program/@name").get(0);
            final Path target = CompileMojo.resolve(temp, name);
            new Xsline(
                input,
                new OutputTo(CompileMojo.resolve(temp, name)),
                new TargetSpy(CompileMojo.resolve(pre, name)),
                new ListOf<>(
                    "org/eolang/maven/pre/classes.xsl",
                    "org/eolang/maven/pre/junit.xsl",
                    "org/eolang/maven/pre/attrs.xsl",
                    "org/eolang/maven/pre/varargs.xsl",
                    "org/eolang/maven/pre/arrays.xsl",
                    "org/eolang/maven/pre/data.xsl",
                    "org/eolang/maven/pre/to-java.xsl"
                )
            ).pass();
            final XML after = this.noErrors(new XMLDocument(target), name);
            for (final XML java : after.nodes("//class[java and not(@atom)]")) {
                new Save(
                    java.xpath("java/text()").get(0),
                    this.generatedDir.toPath().resolve(
                        Paths.get(
                            String.format(
                                "%s.java",
                                java.xpath("@java-name").get(0)
                                    .replace(".", "/")
                            )
                        )
                    )
                ).save();
            }
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "Can't pass %s into %s",
                    file, this.generatedDir
                ),
                ex
            );
        }
        Logger.info(this, "%s compiled to %s", file, this.generatedDir);
    }

    /**
     * Compile one XML file.
     *
     * @param file XML file
     */
    private void compileHSE(final Path file) {
        File xmlFile = new File(file.toUri());
        XML2MediumParser xml = new XML2MediumParser(xmlFile);
        try {
            EOSourceEntity smth = xml.parse();
            ArrayList<EOTargetFile> code = Medium2TargetTranspiler.transpile((EOSourceFile) smth);
            code.forEach(javaFile -> {
                try {
                    new Save(
                            javaFile.getContents(),
                            this.generatedDir.toPath().resolve(
                                    Paths.get(
                                            javaFile.getFileName()
                                    )
                            )
                    ).save();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            });

        } catch (XML2MediumParser.XML2MediumParserException e) {
            e.printStackTrace();
        }

        Logger.info(this, "%s compiled to %s", file, this.generatedDir);
    }

    /**
     * Check for errors.
     *
     * @param xml The XML output
     * @param name Name of the program
     * @return The same XML if no errors
     */
    private XML noErrors(final XML xml, final String name) {
        final List<XML> errors = xml.nodes("/program/errors/error");
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
                    errors.size(), name
                )
            );
        }
        return xml;
    }

    /**
     * Make a relative path.
     * @param dir The dir
     * @param name The name
     * @return Path
     */
    private static Path resolve(final Path dir, final String name) {
        final Path path = dir.resolve(
            String.format(
                "%s.xml",
                name
            )
        );
        if (path.toFile().getParentFile().mkdirs()) {
            Logger.info(CompileMojo.class, "%s directory created", dir);
        }
        return path;
    }

}
