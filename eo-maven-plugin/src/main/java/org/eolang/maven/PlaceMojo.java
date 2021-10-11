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
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.io.InputOf;
import org.eolang.tojos.MonoTojos;
import org.eolang.tojos.Tojos;

/**
 * Take binary files from where ResolveMojo placed them and
 * copy to target/classes.
 *
 * @since 0.11
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "place",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class PlaceMojo extends SafeMojo {

    /**
     * Output.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.outputDirectory}"
    )
    private File outputDir;

    /**
     * The path to a text file where paths of all added
     * .class (and maybe others) files are placed.
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.11.0
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo-placed.csv"
    )
    private File placed;

    @Override
    public void exec() throws IOException {
        final Path home = this.targetDir.toPath().resolve(ResolveMojo.DIR);
        if (Files.exists(home)) {
            final Collection<Path> deps = Files.list(home).collect(Collectors.toList());
            final Tojos tojos = new MonoTojos(this.placed);
            int copied = 0;
            for (final Path dep : deps) {
                for (final Path file : new Walk(dep)) {
                    final String path = file.toString().substring(dep.toString().length() + 1);
                    if (path.startsWith(CopyMojo.DIR)) {
                        continue;
                    }
                    final Path target = this.outputDir.toPath().resolve(path);
                    if (Files.exists(target)) {
                        continue;
                    }
                    new Save(new InputOf(file), target).save();
                    tojos.add(target.toString());
                    ++copied;
                }
            }
            Logger.info(
                this, "Placed %d binary files found in %d dependencies",
                copied, deps.size()
            );
        }
    }

}
