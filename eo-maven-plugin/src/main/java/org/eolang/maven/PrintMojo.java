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
import com.jcabi.xml.XMLDocument;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Walk;
import org.eolang.parser.Schema;
import org.eolang.parser.xmir.Xmir;
import org.eolang.parser.xmir.XmirReversed;
import org.eolang.parser.xmir.XmirSwap;

/**
 * Print XMIR to EO.
 * @since 0.33.0
 */
@Mojo(
    name = "print",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class PrintMojo extends SafeMojo {
    /**
     * Directory with XMIR sources to print.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.printSourcesDir",
        required = true,
        defaultValue = "${project.basedir}/src/main/xmir"
    )
    private File printSourcesDir;

    /**
     * Directory where printed EO files are placed.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.printOutputDir",
        required = true,
        defaultValue = "${project.build.directory}/generated-sources/eo"
    )
    private File printOutputDir;

    /**
     * Print EO in reversed notation.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.printReversed",
        required = true,
        defaultValue = "false"
    )
    private boolean printReversed;

    @Override
    void exec() throws IOException {
        final Home home = new HmBase(this.printOutputDir);
        final int total = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    source -> () -> {
                        final Path relative = Paths.get(
                            this.printSourcesDir.toPath().relativize(source).toString()
                                .replace(".xmir", ".eo")
                        );
                        final XML xml = new XMLDocument(new TextOf(source).asString());
                        new Schema(xml).check();
                        home.save(
                            new XmirSwap(
                                this.printReversed,
                                new XmirReversed(xml),
                                new Xmir.Default(xml)
                            ).toEO(),
                            relative
                        );
                        Logger.info(
                            this,
                            "Printed: %[file]s (%[size]s) => %[file]s (%[size]s)",
                            source,
                            source.toFile().length(),
                            this.printOutputDir.toPath().resolve(relative),
                            this.printOutputDir.toPath().resolve(relative).toFile().length()
                        );
                        return 1;
                    },
                    new Walk(this.printSourcesDir.toPath())
                )
            )
        ).intValue();
        if (total == 0) {
            Logger.info(this, "No XMIR sources found");
        } else {
            Logger.info(this, "Printed %d XMIR sources into EO", total);
        }
    }
}
