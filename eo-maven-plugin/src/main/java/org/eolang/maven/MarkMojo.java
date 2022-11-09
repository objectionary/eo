/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import com.yegor256.tojos.Tojo;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Extend current list of foreign objects with those
 * visible in resolved artifacts.
 *
 * @since 0.11
 */
@Mojo(
    name = "extend",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MarkMojo extends SafeMojo {

    @Override
    @SuppressWarnings({ "PMD.GuardLogStatement", "PMD.PrematureDeclaration" })
    public void exec() throws IOException {
        final Path home = this.targetDir.toPath().resolve(ResolveMojo.DIR);
        if (Files.exists(home)) {
            final Collection<String> deps = new DepDirs(home);
            int found = 0;
            for (final String dep : deps) {
                final Path dir = home.resolve(dep);
                final Path sub = dir.resolve(CopyMojo.DIR);
                if (!Files.exists(sub)) {
                    continue;
                }
                final String ver = dep.split(Pattern.quote(File.separator))[3];
                found += this.scan(sub, ver);
            }
            Logger.info(
                this, "New %d objects found in %d unpacked dependencies",
                found, deps.size()
            );
        }
    }

    /**
     * Take sources from EO-SOURCES dir and register them in the CSV.
     *
     * @param dir Where they are
     * @param version The version of the JAR
     * @return How many registered
     * @throws IOException If fails
     * @todo #1062:30min The mojo doesn't update program version if it exists.
     *  This causes versions like `*.*.*` and `0.0.0` are not updated and remain
     *  in foreign catalog. This needs to be updated: version must be overridden to
     *  correct value.
     */
    private int scan(final Path dir, final String version) throws IOException {
        final Unplace unplace = new Unplace(dir);
        final Collection<Path> sources = new Walk(dir);
        int done = 0;
        for (final Path src : sources) {
            if (src.toString().endsWith(".eo")) {
                final Tojo tojo = this.scopedTojos().add(unplace.make(src));
                if (!tojo.exists(AssembleMojo.ATTR_VERSION)) {
                    tojo.set(AssembleMojo.ATTR_VERSION, version);
                }
                ++done;
            }
        }
        Logger.info(
            this, "Found %d sources in %s, %d program(s) registered with version %s",
            sources.size(), new Rel(dir), done, version
        );
        return done;
    }

}
