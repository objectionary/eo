/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;
import org.eolang.parser.Phi;
import org.xembly.Directive;

/**
 * Read PHI files and parse them to the XMIR.
 * @since 0.34.0
 */
@Mojo(
    name = "phi-to-xmir",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjUnphi extends MjSafe {
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
        final List<String> errors = new CopyOnWriteArrayList<>();
        final Path input = this.unphiInputDir.toPath();
        final Path output = this.unphiOutputDir.toPath();
        final Iterable<Directive> metas = new Phi.Metas(this.unphiMetas);
        final long start = System.currentTimeMillis();
        final int count = new Threaded<>(
            new Walk(input),
            phi -> {
                final Path xmir = Paths.get(
                    input.relativize(phi).toString().replace(
                        String.format(".%s", MjPhi.EXT),
                        String.format(".%s", MjAssemble.XMIR)
                    )
                );
                final Path target = output.resolve(xmir);
                new FpDefault(
                    source -> {
                        final XML result = new Phi(source, metas).unphi();
                        final List<String> here = MjUnphi.errors(result);
                        if (!here.isEmpty()) {
                            errors.add(
                                Logger.format(
                                    "%[file]s:\n\t%s\n",
                                    xmir,
                                    String.join("\n\t", here)
                                )
                            );
                        }
                        return result.toString();
                    },
                    this.cache.toPath().resolve("unphied"),
                    this.plugin.getVersion(),
                    this.hash.value(),
                    xmir
                ).apply(phi, target);
                Logger.debug(
                    this,
                    "Parsed to xmir: %[file]s -> %[file]s",
                    phi, target
                );
                return 1;
            }
        ).total();
        Logger.info(
            this,
            "Parsed %d phi files to xmir in %[ms]s",
            count, System.currentTimeMillis() - start
        );
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
     * Get errors list.
     * @param xml XML
     * @return Errors list
     */
    private static List<String> errors(final XML xml) {
        return new Xnav(xml.inner())
            .element("object")
            .elements(Filter.withName("errors"))
            .findFirst()
            .map(
                errors -> errors.elements(Filter.withName("error"))
                    .map(xnav -> xnav.text().get())
                    .collect(Collectors.toList())
            ).orElse(List.of());
    }
}
