/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.IterableEnvelope;
import org.cactoos.iterable.Joined;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.number.SumOf;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Walk;
import org.eolang.parser.PhiSyntax;
import org.eolang.parser.TrStepped;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Read PHI files and parse them to the XMIR.
 * @since 0.34.0
 */
@Mojo(
    name = "phi-to-xmir",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class UnphiMojo extends SafeMojo {
    /**
     * Unphi transformations.
     */
    private static final Train<Shift> TRANSFORMATIONS = new TrStepped(
        new TrClasspath<>(
            "/org/eolang/maven/unphi/wrap-bytes.xsl",
            "/org/eolang/parser/wrap-method-calls.xsl",
            "/org/eolang/maven/unphi/atoms-with-bound-attrs.xsl"
        ).back()
    );

    /**
     * The directory where to take phi files for parsing from.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.unphiInputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/phi"
    )
    private File unphiInputDir;

    /**
     * The directory where to save xmir files to.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.unphiOutputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/1-parse"
    )
    private File unphiOutputDir;

    /**
     * Extra metas to add to unphied XMIR.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.unphiMetas")
    private Set<String> unphiMetas = new SetOf<>();

    @Override
    public void exec() {
        final List<String> errors = new ListOf<>();
        final Home home = new HmBase(this.unphiOutputDir);
        final Iterable<Directive> metas = new UnphiMojo.Metas(this.unphiMetas);
        final int count = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    phi -> () -> {
                        final Path relative = this.unphiInputDir.toPath().relativize(phi);
                        final Path xmir = Paths.get(
                            relative.toString().replace(
                                String.format(".%s", PhiMojo.EXT),
                                String.format(".%s", AssembleMojo.XMIR)
                            )
                        );
                        final XML result = new Xsline(this.measured(UnphiMojo.TRANSFORMATIONS))
                            .pass(
                                new PhiSyntax(
                                    phi.getFileName().toString().replace(".phi", ""),
                                    new TextOf(phi),
                                    metas
                                ).parsed()
                            );
                        home.save(result.toString(), xmir);
                        Logger.info(
                            this,
                            "Parsed to xmir: %[file]s -> %[file]s",
                            phi, this.unphiOutputDir.toPath().resolve(xmir)
                        );
                        if (result.nodes("//errors[count(error)=0]").isEmpty()) {
                            errors.add(
                                String.format(
                                    "%s:\n\t%s\n",
                                    relative,
                                    String.join("\n\t", result.xpath("//errors/error/text()"))
                                )
                            );
                        }
                        return 1;
                    },
                    new Walk(this.unphiInputDir.toPath())
                )
            )
        ).intValue();
        Logger.info(this, "Parsed %d phi files to xmir", count);
        if (!errors.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "%d files with parsing errors were found:\n%s",
                    errors.size(),
                    String.join("\n", errors)
                )
            );
        }
    }

    /**
     * Accumulates all metas that should be attached to unphied XMIR.
     * +package meta is prohibited since it's converted to special object
     * with "λ -> Package" binding.
     * @since 0.36.0
     */
    private static class Metas extends IterableEnvelope<Directive> {
        /**
         * Package meta.
         */
        private static final String PACKAGE = "package";

        /**
         * Ctor.
         * @param metas Metas to append
         */
        Metas(final Iterable<String> metas) {
            super(
                new Joined<>(
                    new Mapped<>(
                        meta -> {
                            final String[] pair = meta.split(" ", 2);
                            final String head = pair[0].substring(1);
                            if (UnphiMojo.Metas.PACKAGE.equals(head)) {
                                throw new IllegalStateException(
                                    "+package meta is prohibited for attaching to unphied XMIR"
                                );
                            }
                            final Directives dirs = new Directives()
                                .xpath("/program/metas")
                                .add("meta")
                                .add("head").set(head).up()
                                .add("tail");
                            if (pair.length > 1) {
                                dirs.set(pair[1].trim()).up();
                                for (final String part : pair[1].trim().split(" ")) {
                                    dirs.add("part").set(part).up();
                                }
                            } else {
                                dirs.up();
                            }
                            return dirs.up();
                        },
                        metas
                    )
                )
            );
        }
    }
}
