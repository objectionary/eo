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
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.io.InputOf;
import org.cactoos.scalar.Unchecked;
import org.cactoos.set.SetOf;
import org.eolang.maven.tojos.PlacedTojo;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.HmOptional;
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

    /**
     * Place only binaries that have EO sources inside jar.
     * @since 0.31
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    @SuppressWarnings("PMD.LongVariable")
    private boolean placeBinariesThatHaveSources;

    @Override
    public void exec() throws IOException {
        final Path home = this.targetDir.toPath().resolve(ResolveMojo.DIR);
        if (Files.exists(home)) {
            final Collection<String> deps = new DepDirs(home);
            final long copied = deps.stream()
                .mapToLong(dep -> this.placeDependency(home, dep))
                .sum();
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
     * @since 0.30
     */
    private long placeDependency(final Path home, final String dep) {
        if (this.placedTojos.findJar(dep).isPresent()) {
            Logger.info(this, "Found placed binaries from %s", dep);
        }
        final Path dir = home.resolve(dep);
        final long copied = new BinariesDependency(dir, dep, this.rewriteBinaries).place();
        this.placedTojos.placeJar(dep);
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
     * Dependency which binaries we are going to place.
     *
     * @since 0.30
     */
    private final class BinariesDependency {

        /**
         * Directory to read from.
         */
        private final Path dir;

        /**
         * Dependency name.
         */
        private final String dep;

        /**
         * Rewrite binary or not.
         */
        private final boolean rewrite;

        /**
         * Ctor.
         * @param directory The directory to read from
         * @param dependency The name of dependency
         * @param rwte Rewrite binaries in output directory or not
         */
        private BinariesDependency(
            final Path directory,
            final String dependency,
            final boolean rwte
        ) {
            this.dir = directory;
            this.dep = dependency;
            this.rewrite = rwte;
        }

        /**
         * Place all binaries from this dependency.
         * @return How many binaries placed
         */
        private long place() {
            return new Walk(this.dir)
                .includes(PlaceMojo.this.includeBinaries)
                .excludes(PlaceMojo.this.excludeBinaries)
                .stream()
                .filter(this::isNotEoSource)
                .filter(this::isNotAlreadyPlaced)
                .filter(this::hasEoSource)
                .peek(this::printLogInfoAboutBinary)
                .peek(this::placeBinary)
                .count();
        }

        /**
         * Check if the file is not a source file.
         * @param file The file to check.
         * @return True if the file is not a source file.
         */
        private boolean isNotEoSource(final Path file) {
            final boolean res;
            if (this.dir.relativize(file).startsWith(CopyMojo.DIR)) {
                Logger.debug(
                    this,
                    "File %s is not a binary, but a source, won't place it",
                    new Rel(file)
                );
                res = false;
            } else {
                res = true;
            }
            return res;
        }

        /**
         * Check whether the binary file has corresponding EO sources in the jar.
         * The method checks ONLY EO binaries and classes. All other java files or classes in jar
         * will be included anyway.
         * Let's consider the next filesystem structure.
         * Source file:
         * - "EO-SOURCE/org/eolang/txt/x.eo" -
         *
         * Correct:
         * - "EOorg/EOeolang/EOtxt/x.class" - is correct since has corresponding EO source folder
         * - "EOorg/EOeolang/EOtxt/y&z.class" - is correct since has corresponding EO source folder
         * - "com/sun/jna/Callback.class" - is correct since binary file is not in EOorg folder
         *
         * Is incorrect (since has no corresponding EO source folder):
         * - "EOorg/EOeolang/EObool.class"
         * - "EOorg/x.class"
         *
         * The filter is disabled by default, works only if the parameter
         * "placeBinariesThatHaveSources" is set to true.
         *
         * @param file The file to check.
         * @return True if the file has corresponding EO sources.
         */
        private boolean hasEoSource(final Path file) {
            final boolean result;
            if (PlaceMojo.this.placeBinariesThatHaveSources && file.toString().contains("EOorg")) {
                final Path sources = this.dir.resolve(CopyMojo.DIR)
                    .resolve(this.dir.relativize(file.getParent()).toString().replace("EO", ""));
                result = Files.exists(sources)
                    && Files.isDirectory(sources)
                    && Arrays.stream(sources.toFile().listFiles())
                    .filter(Objects::nonNull)
                    .filter(File::isFile).count() > 0;
            } else {
                result = true;
            }
            return result;
        }

        /**
         * Check if the file is not already placed.
         * @param file The file to check.
         * @return True if the file is not already placed.
         */
        private boolean isNotAlreadyPlaced(final Path file) {
            final Path target = PlaceMojo.this.outputDir.toPath().resolve(
                this.dir.relativize(file)
            );
            final Optional<PlacedTojo> tojo = PlaceMojo.this.placedTojos.find(target);
            final boolean res;
            if (tojo.isPresent() && Files.exists(target)
                && (this.sameLength(target, file) || !tojo.get().unplaced())) {
                Logger.debug(
                    this,
                    "The same file %s is already placed to %s maybe by %s, skipping",
                    new Rel(file),
                    new Rel(target),
                    tojo.get().dependency()
                );
                res = false;
            } else {
                res = true;
            }
            return res;
        }

        /**
         * Print log info about placing class.
         * @param file The file to place.
         */
        private void printLogInfoAboutBinary(final Path file) {
            final Path target = PlaceMojo.this.outputDir.toPath().resolve(
                this.dir.relativize(file)
            );
            final Optional<PlacedTojo> tojo = PlaceMojo.this.placedTojos.find(target);
            if (tojo.isPresent()) {
                if (!Files.exists(target)) {
                    Logger.info(
                        this,
                        "The file %s has been placed to %s, but now it's gone, replacing",
                        new Rel(file),
                        new Rel(target)
                    );
                }
                if (Files.exists(target) && !this.sameLength(target, file)) {
                    Logger.debug(
                        this,
                        "File %s (%d bytes) was already placed at %s (%d bytes!) by %s, replacing",
                        new Rel(file), file.toFile().length(),
                        new Rel(target), target.toFile().length(),
                        tojo.get().dependency()
                    );
                }
            }
        }

        /**
         * Place class.
         * @param file File to place
         */
        private void placeBinary(final Path file) {
            final Path path = this.dir.relativize(file);
            try {
                final Path target = PlaceMojo.this.outputDir.toPath().resolve(path);
                new HmOptional(
                    new HmBase(PlaceMojo.this.outputDir),
                    this.rewrite
                ).save(new InputOf(file), path);
                PlaceMojo.this.placedTojos.placeClass(
                    target,
                    PlaceMojo.this.outputDir.toPath().relativize(target).toString(),
                    this.dep
                );
            } catch (final IOException ex) {
                throw new IllegalStateException(
                    String.format(
                        "Failed to place %s to home %s with path %s",
                        file, PlaceMojo.this.outputDir, path
                    ), ex
                );
            }
        }

        /**
         * Check if two files have the same length.
         * @param first First file
         * @param second Second file
         * @return True if they have the same length
         * @checkstyle NonStaticMethodCheck (2 lines)
         */
        private boolean sameLength(final Path first, final Path second) {
            return new Unchecked<>(() -> Files.size(first) == Files.size(second)).value();
        }
    }
}
