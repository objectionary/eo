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
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Set;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.io.InputOf;
import org.cactoos.set.SetOf;
import org.eolang.maven.util.FileHash;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Rel;
import org.eolang.maven.util.Walk;

/**
 * Take binary files from where ResolveMojo placed them and
 * copy to target/classes.
 *
 * @since 0.11
 * @see <a href="https://news.eolang.org/2022-10-19-placed-catalog.html">Place catalog</a>
 */
@Mojo(
    name = "place",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
@SuppressWarnings({"PMD.ImmutableField", "PMD.AvoidDuplicateLiterals"})
public final class PlaceMojo extends SafeMojo {

    /**
     * Attr in CSV.
     */
    static final String ATTR_PLD_RELATED = "related";

    /**
     * Attr in CSV.
     */
    static final String ATTR_PLD_KIND = "kind";

    /**
     * Attr in CSV.
     */
    static final String ATTR_PLD_HASH = "hash";

    /**
     * Where the binary is coming from (JAR name).
     */
    static final String ATTR_PLD_DEP = "dependency";

    /**
     * Attr in CSV.
     */
    static final String ATTR_PLD_UNPLACED = "unplaced";

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
     * List of inclusion GLOB filters for finding class files.
     * @since 0.15
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private Set<String> includeBinaries = new SetOf<>("**");

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
                final Collection<Tojo> before = this.placedTojos.value().select(
                    row -> row.get(Tojos.KEY).equals(dep)
                        && "jar".equals(row.get(PlaceMojo.ATTR_PLD_KIND))
                );
                if (!before.isEmpty()) {
                    Logger.info(this, "Found placed binaries from %s", dep);
                }
                copied += this.place(home, dep);
                this.placedTojos.value().add(dep).set(PlaceMojo.ATTR_PLD_KIND, "jar");
            }
            if (copied == 0) {
                Logger.debug(
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
                new Rel(home)
            );
        }
    }

    /**
     * Place one dep.
     * @param home Home to read from
     * @param dep The name of dep
     * @return How many binaries placed
     * @throws IOException If fails
     * @checkstyle ExecutableStatementCountCheck (300 lines)
     * @checkstyle CyclomaticComplexityCheck (300 lines)
     * @checkstyle NPathComplexityCheck (300 lines)
     * @todo #1320:30min Refactor PlaceMojo#place method in order to reduce complexity.
     *  When the method will be refactored we have to remove all warning suppressing from
     *  checkstyle and PMD.
     */
    @SuppressWarnings("PMD.NPathComplexity")
    private int place(final Path home, final String dep) throws IOException {
        final Path dir = home.resolve(dep);
        final Collection<Path> binaries = new Walk(dir)
            .includes(this.includeBinaries)
            .excludes(this.excludeBinaries);
        int copied = 0;
        for (final Path file : binaries) {
            final String path = file.toString().substring(dir.toString().length() + 1);
            if (path.startsWith(CopyMojo.DIR)) {
                Logger.debug(
                    this,
                    "File %s is not a binary, but a source, won't place it",
                    new Rel(file)
                );
                continue;
            }
            final Path target = this.outputDir.toPath().resolve(path);
            final Collection<Tojo> before = this.placedTojos.value().select(
                row -> row.get(Tojos.KEY).equals(target.toString())
                    && "class".equals(row.get(PlaceMojo.ATTR_PLD_KIND))
            );
            if (!before.isEmpty() && !Files.exists(target)) {
                Logger.info(
                    this,
                    "The file %s has been placed to %s, but now it's gone, re-placing",
                    new Rel(file),
                    new Rel(target)
                );
            }
            if (!before.isEmpty() && Files.exists(target)
                && target.toFile().length() == file.toFile().length()) {
                Logger.debug(
                    this,
                    "The same file %s is already placed to %s maybe by %s, skipping",
                    new Rel(file), new Rel(target),
                    before.iterator().next().get(PlaceMojo.ATTR_PLD_DEP)
                );
                continue;
            }
            if (!before.isEmpty() && Files.exists(target)
                && target.toFile().length() != file.toFile().length()) {
                Logger.debug(
                    this,
                    "File %s (%d bytes) was already placed at %s (%d bytes!) by %s, replacing",
                    new Rel(file), file.toFile().length(),
                    new Rel(target), target.toFile().length(),
                    before.iterator().next().get(PlaceMojo.ATTR_PLD_DEP)
                );
            }
            if (!before.isEmpty() && Files.exists(target) && PlaceMojo.isNotUnplaced(before)) {
                continue;
            }
            new Home(this.outputDir.toPath()).save(new InputOf(file), Paths.get(path));
            this.placedTojos.value().add(target.toString())
                .set(PlaceMojo.ATTR_PLD_KIND, "class")
                .set(PlaceMojo.ATTR_PLD_HASH, new FileHash(target))
                .set(
                    PlaceMojo.ATTR_PLD_RELATED,
                    target.toString().substring(
                        this.outputDir.toString().length() + 1
                    )
                )
                .set(PlaceMojo.ATTR_PLD_DEP, dep)
                .set(PlaceMojo.ATTR_PLD_UNPLACED, "false");
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

    /**
     * Check whether tojos was not unplaced.
     * @param before Tojos.
     * @return True if not unplaced.
     */
    private static boolean isNotUnplaced(final Iterable<? extends Tojo> before) {
        final Tojo tojo = before.iterator().next();
        return tojo.exists(PlaceMojo.ATTR_PLD_UNPLACED)
            && "false".equals(tojo.get(PlaceMojo.ATTR_PLD_UNPLACED));
    }
}
