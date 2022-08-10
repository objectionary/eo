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
    public static final String ATTR_RELATED = "related";

    /**
     * Attr in CSV.
     */
    public static final String ATTR_KIND = "kind";

    /**
     * Attr in CSV.
     */
    public static final String ATTR_HASH = "hash";

    /**
     * Where the binary is coming from (JAR name).
     */
    public static final String ATTR_ORIGIN = "dependency";

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
                final Collection<Tojo> before = this.catalog().select(
                    row -> row.get(Tojos.KEY).equals(dep)
                        && "jar".equals(row.get(PlaceMojo.ATTR_KIND))
                );
                if (!before.isEmpty()) {
                    Logger.info(this, "Binaries from %s have already been placed", dep);
                    continue;
                }
                copied += this.place(home, dep);
                this.catalog().add(dep).set(PlaceMojo.ATTR_KIND, "jar");
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
     * @checkstyle ExecutableStatementCountCheck (200 lines)
     * @checkstyle CyclomaticComplexityCheck (200 lines)
     */
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
                    Save.rel(file)
                );
                continue;
            }
            final Path target = this.outputDir.toPath().resolve(path);
            final Collection<Tojo> before = this.catalog().select(
                row -> row.get(Tojos.KEY).equals(target.toString())
                    && "class".equals(row.get(PlaceMojo.ATTR_KIND))
            );
            if (!before.isEmpty() && !Files.exists(target)) {
                throw new IllegalStateException(
                    String.format(
                        "The file %s has been placed to %s, but now it's gone",
                        Save.rel(file), Save.rel(target)
                    )
                );
            }
            if (!before.isEmpty() && Files.exists(target)
                && target.toFile().length() == file.toFile().length()) {
                Logger.warn(
                    this,
                    "The same file %s is already placed to %s maybe by %s, skipping",
                    Save.rel(file), Save.rel(target),
                    before.iterator().next().get(PlaceMojo.ATTR_ORIGIN)
                );
                continue;
            }
            if (!before.isEmpty() && Files.exists(target)
                && target.toFile().length() != file.toFile().length()) {
                Logger.warn(
                    this,
                    "File %s (%d bytes) was already placed at %s (%d bytes!) by %s, replacing",
                    Save.rel(file), file.toFile().length(),
                    Save.rel(target), target.toFile().length(),
                    before.iterator().next().get(PlaceMojo.ATTR_ORIGIN)
                );
            }
            new Save(new InputOf(file), target).save();
            this.catalog().add(target.toString())
                .set(PlaceMojo.ATTR_KIND, "class")
                .set(PlaceMojo.ATTR_HASH, new FileHash(target))
                .set(
                    PlaceMojo.ATTR_RELATED,
                    target.toString().substring(
                        this.outputDir.toString().length() + 1
                    )
                )
                .set(PlaceMojo.ATTR_ORIGIN, dep);
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
     * Get catalog.
     * @return Tojos
     */
    private Tojos catalog() {
        return new Catalog(this.placed.toPath(), this.placedFormat).make();
    }

}
