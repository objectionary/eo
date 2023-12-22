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
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Text;
import org.cactoos.experimental.Threads;
import org.cactoos.io.ResourceOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.cactoos.text.Sticky;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Walk;

/**
 * Read XMIR files and translate them to the phi-calculus expression.
 * @since 0.34.0
 */
@Mojo(
    name = "xmir-to-phi",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class PhiMojo extends SafeMojo {
    /**
     * Extension of the file where we put phi-calculus expression (.phi).
     */
    public static final String EXT = "phi";

    /**
     * Translation to phi.
     */
    private static final Text TRANSLATION = new Sticky(
        new TextOf(
            new ResourceOf("org/eolang/maven/phi/to-phi.xsl")
        )
    );

    /**
     * The directory where to take xmir files for translation from.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "phiInputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/2-optimize"
    )
    private File phiInputDir;

    /**
     * The directory where to save phi files to.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "phiOutputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/phi"
    )
    private File phiOutputDir;

    @Override
    public void exec() {
        final Home home = new HmBase(this.phiOutputDir);
        final int count = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    xmir -> () -> {
                        final Path relative = Paths.get(
                            this.phiInputDir.toPath().relativize(xmir).toString().replace(
                                String.format(".%s", TranspileMojo.EXT),
                                String.format(".%s", PhiMojo.EXT)
                            )
                        );
                        home.save(PhiMojo.translated(new TextOf(xmir)), relative);
                        Logger.info(
                            this,
                            "Translated to phi: %s -> %s",
                            xmir, this.phiOutputDir.toPath().resolve(relative)
                        );
                        return 1;
                    },
                    new Walk(this.phiInputDir.toPath())
                )
            )
        ).intValue();
        if (count > 0) {
            Logger.info(this, "Translated %d XMIR file(s) to PHI", count);
        } else {
            Logger.info(this, "No XMIR files translated to PHI");
        }
    }

    /**
     * Translate given xmir to phi calculus expression.
     * @param xmir Text of xmir
     * @return Translated xmir
     * @throws Exception If fail to translate
     */
    private static String translated(final Text xmir) throws Exception {
        return new XSLDocument(PhiMojo.TRANSLATION.asString()).applyTo(
            new XMLDocument(xmir.asString())
        );
    }
}
