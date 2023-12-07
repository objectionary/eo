/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import java.io.File;
import java.nio.charset.StandardCharsets;
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
import org.eolang.parser.PhiSyntax;

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
     * The directory where to take phi files for parsing from.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "unphiInputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/phi"
    )
    private File unphiInputDir;

    /**
     * The directory where to save xmir files to.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "unphiOutputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/1-parse"
    )
    private File unphiOutputDir;

    @Override
    public void exec() {
        final Home home = new HmBase(this.unphiOutputDir);
        final int count = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    phi -> () -> {
                        final Path relative = Paths.get(
                            this.unphiInputDir.toPath().relativize(phi).toString().replace(
                                String.format(".%s", PhiMojo.EXT),
                                String.format(".%s", TranspileMojo.EXT)
                            )
                        );
                        home.save(
                            new PhiSyntax(
                                phi.getFileName().toString().replace(".phi", ""),
                                new TextOf(phi)
                            ).parsed().toString(),
                            relative
                        );
                        Logger.info(
                            this,
                            "Parsed to xmir: %s -> %s",
                            phi, this.unphiOutputDir.toPath().resolve(relative)
                        );
                        return 1;
                    },
                    new Walk(this.unphiInputDir.toPath())
                )
            )
        ).intValue();
        Logger.info(this, "Parsed %d phi files to xmir", count);
    }
}
