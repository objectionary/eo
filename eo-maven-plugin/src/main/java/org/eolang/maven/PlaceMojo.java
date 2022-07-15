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
import com.yegor256.tojos.Tojos;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Set;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.io.InputOf;
import org.cactoos.set.SetOf;

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
@SuppressWarnings("PMD.ImmutableField")
public final class PlaceMojo extends SafeMojo {

    /**
     * Output.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.outputDir",
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
        property = "eo.placed",
        required = true,
        defaultValue = "${project.build.directory}/eo/placed.csv"
    )
    private File placed;

    /**
     * Format of "placed" file ("json" or "csv").
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    @Parameter(property = "eo.placedFormat", required = true, defaultValue = "csv")
    private String placedFormat = "csv";

    /**
     * List of inclusion GLOB filters for finding class files.
     * @since 0.15
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private Set<String> includeBinaries = new SetOf<>("EO**", "org/eolang/**");

    /**
     * List of exclusion GLOB filters for finding class files.
     * @since 0.15
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private Set<String> excludeBinaries = new SetOf<>();

    @Override
    public void exec() throws IOException {
        final Path home = this.targetDir.toPath().resolve(ResolveMojo.DIR);
        if (Files.exists(home)) {
            final Collection<String> deps = new DepDirs(home);
            int copied = 0;
            for (final String dep : deps) {
                copied += this.place(home, dep);
            }
            if (copied == 0) {
                Logger.info(
                    this, "No binary files placed from %d dependencies",
                    deps.size()
                );
            } else {
                Logger.info(
                    this, "Placed %d binary files found in %d dependencies",
                    copied, deps.size()
                );
            }
        } else {
            Logger.info(
                this, "The directory is absent, nothing to place: %s",
                Save.rel(home)
            );
        }
    }

    /**
     * Place one dep.
     * @param home Home to read from
     * @param dep The name of dep
     * @return How many binaries placed
     * @throws IOException If fails
     */
    private int place(final Path home, final String dep) throws IOException {
        final Path dir = home.resolve(dep);
        final Collection<Path> binaries = new Walk(dir)
            .includes(this.includeBinaries)
            .excludes(this.excludeBinaries);
        int copied = 0;
        final Tojos tojos = new Catalog(this.placed.toPath(), this.placedFormat).make();
        for (final Path file : binaries) {
            final String path = file.toString().substring(dir.toString().length() + 1);
            if (path.startsWith(CopyMojo.DIR)) {
                Logger.debug(
                    this, "File %s is not a binary, but a source, won't place it",
                    Save.rel(file)
                );
                continue;
            }
            final Path target = this.outputDir.toPath().resolve(path);
            if (Files.exists(target)) {
                if (target.toFile().length() == file.toFile().length()) {
                    Logger.debug(
                        this, "File %s is already placed to %s",
                        Save.rel(file), Save.rel(target)
                    );
                } else {
                    Logger.warn(
                        this, "File %s (%d bytes) is already placed to %s (%d bytes!)",
                        Save.rel(file), file.toFile().length(),
                        Save.rel(target), target.toFile().length()
                    );
                }
                continue;
            }
            new Save(new InputOf(file), target).save();
            tojos.add(target.toString());
            ++copied;
        }
        if (copied > 0) {
            Logger.info(
                this, "Placed %d binary file(s) out of %d, found in %s",
                copied, new Walk(dir).size(), dep
            );
        } else {
            Logger.info(
                this, "No binary file(s) out of %d were placed from %s",
                new Walk(dir).size(), dep
            );
        }
        return copied;
    }

}
