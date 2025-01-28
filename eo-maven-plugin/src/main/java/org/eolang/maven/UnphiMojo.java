/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Scalar;
import org.cactoos.set.SetOf;
import org.eolang.maven.footprint.FpDefault;
import org.eolang.maven.util.Threaded;
import org.eolang.maven.util.Walk;
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
public final class UnphiMojo extends SafeMojo {
    /**
     * Subdirectory for parsed cache.
     */
    static final String CACHE = "unphied";

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
        final Iterable<Directive> metas = new Phi.Metas(this.unphiMetas);
        final long start = System.currentTimeMillis();
        final int count = new Threaded<>(
            new Walk(this.unphiInputDir.toPath()),
            phi -> this.included(errors, () -> this.unphied(phi, metas))
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
     * Eject errors from parsed xmir file.
     *
     * @param errors List of errors
     * @param parsed Function providing path to parsed xmir
     * @return Always 1
     * @throws Exception When failed to unphi
     */
    private int included(final List<String> errors, final Scalar<Path> parsed) throws Exception {
        final Path xmir = parsed.value();
        final List<String> here = new XMLDocument(xmir).xpath("//errors/error/text()");
        if (!here.isEmpty()) {
            errors.add(
                Logger.format(
                    "%[file]s:\n\t%s\n",
                    this.unphiOutputDir.toPath().relativize(xmir),
                    String.join("\n\t", here)
                )
            );
        }
        return 1;
    }

    /**
     * Parses phi to xmir with cache.
     *
     * @param phi Path to phi
     * @param metas Extra metas to add to unphied XMIR
     * @return Path to produced xmir
     * @throws IOException When failed to unphi
     */
    private Path unphied(
        final Path phi,
        final Iterable<Directive> metas
    ) throws IOException {
        final Path xmir = Paths.get(
            this.unphiInputDir.toPath()
                .relativize(phi)
                .toString()
                .replace(
                    String.format(".%s", PhiMojo.EXT),
                    String.format(".%s", AssembleMojo.XMIR)
                )
        );
        final Path target = new FpDefault(
            ignore -> new Phi(phi, metas).unphi().toString(),
            this.cache.toPath().resolve(UnphiMojo.CACHE),
            this.plugin.getVersion(),
            () -> "",
            xmir
        ).apply(phi, this.unphiOutputDir.toPath().resolve(xmir));
        Logger.debug(
            this,
            "Parsed to xmir: %[file]s -> %[file]s",
            phi, target
        );
        return target;
    }
}
