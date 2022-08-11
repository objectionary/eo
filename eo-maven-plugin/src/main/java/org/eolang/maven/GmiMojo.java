/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrLogged;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

/**
 * Convert XMIR to GMI.
 *
 * @since 0.27
 */
@Mojo(
    name = "gmi",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.ImmutableField")
public final class GmiMojo extends SafeMojo {

    /**
     * The directory where to save GMI to.
     */
    public static final String DIR = "gmi";

    @Override
    public void exec() throws IOException {
        final Collection<Tojo> tojos = this.scopedTojos().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR2)
        );
        int total = 0;
        for (final Tojo tojo : tojos) {
            final Path gmi = new Place(tojo.get(Tojos.KEY)).make(
                this.targetDir.toPath().resolve(GmiMojo.DIR), "gmi.xml"
            );
            final Path xmir = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR));
            if (gmi.toFile().lastModified() >= xmir.toFile().lastModified()) {
                Logger.debug(
                    this, "Already converted %s to %s (it's newer than the source)",
                    tojo.get(Tojos.KEY), Save.rel(gmi)
                );
                continue;
            }
            this.render(xmir, gmi);
            tojo.set(AssembleMojo.ATTR_GMI, gmi.toAbsolutePath().toString());
            ++total;
        }
        if (total == 0) {
            if (tojos.isEmpty()) {
                Logger.info(this, "No .xmir need to be converted to GMIs");
            } else {
                Logger.info(this, "No .xmir converted to GMIs");
            }
        } else {
            Logger.info(this, "Converted %d .xmir to GMIs", total);
        }
    }

    /**
     * Convert XMIR file to GMI.
     *
     * @param xmir Location of XMIR
     * @param gmi Location of GMI
     * @throws IOException If fails
     */
    private void render(final Path xmir, final Path gmi) throws IOException {
        final Train<Shift> train = new TrLogged(
            new TrClasspath<>(
                new TrDefault<>(),
                "/org/eolang/maven/gmi/R0.xsl",
                "/org/eolang/maven/gmi/R1.xsl",
                "/org/eolang/maven/gmi/R3.xsl",
                "/org/eolang/maven/gmi/R6.xsl"
            ).back(),
            GmiMojo.class
        );
        final XML before = new XMLDocument(xmir);
        final XML after = new Xsline(train).pass(before);
        new Save(after.toString(), gmi).save();
        Logger.debug(
            this, "GMI for %s saved to %s (%s chars)",
            Save.rel(xmir), Save.rel(gmi), Files.size(gmi)
        );
    }

}
