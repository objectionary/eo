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
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.list.ListOf;
import org.cactoos.set.SetOf;
import org.eolang.maven.tojos.PlacedTojo;
import org.eolang.maven.util.FileHash;
import org.eolang.maven.util.Rel;

/**
 * It deletes binary files, which were previously copied by "place" mojo.
 *
 * @since 0.11
 * @checkstyle ExecutableStatementCountCheck (500 lines)
 */
@Mojo(
    name = "unplace",
    defaultPhase = LifecyclePhase.PREPARE_PACKAGE,
    threadSafe = true
)
@SuppressWarnings("PMD.ImmutableField")
public final class UnplaceMojo extends SafeMojo {

    /**
     * List of inclusion GLOB filters for unplacing (these files will be removed for sure).
     * @since 0.24
     * @see <a href="https://news.eolang.org/2022-07-15-placing-and-unplacing.html">Placing and Unplacing in JAR Artifacts</a>
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private Set<String> removeBinaries = new SetOf<>();

    /**
     * List of inclusion GLOB filters for placing (ONLY these files will stay).
     * @since 0.24
     * @see <a href="https://news.eolang.org/2022-07-15-placing-and-unplacing.html">Placing and Unplacing in JAR Artifacts</a>
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private Set<String> keepBinaries = new SetOf<>();

    @Override
    public void exec() throws IOException {
        if (this.placedTojos.isEmpty()) {
            Logger.info(
                this,
                "The list of placed binaries is absent: %s",
                new Rel(this.placed)
            );
        } else {
            this.unplaceClasses();
            this.unplaceJars();
        }
    }

    /**
     * Mark dependencies as unplaced if all related binaries are unplaced.
     */
    private void unplaceJars() {
        final Set<String> used = this.placedTojos.classes()
            .stream()
            .map(PlacedTojo::dependency)
            .collect(Collectors.toSet());
        this.placedTojos.jars().stream()
            .filter(dep -> used.contains(dep.identifier()))
            .forEach(PlacedTojo::unplace);
    }

    /**
     * Place what's necessary.
     * @throws IOException If fails
     */
    private void unplaceClasses() throws IOException {
        final Collection<PlacedTojo> classes = this.placedTojos.classes();
        int deleted = 0;
        if (!this.keepBinaries.isEmpty()) {
            deleted += this.keepThem(classes);
        }
        deleted += this.killThem(classes);
        if (classes.isEmpty()) {
            Logger.info(
                this, "No binaries were placed into %s, nothing to uplace",
                new Rel(this.placed)
            );
        } else if (deleted == 0) {
            Logger.info(
                this, "No binaries out of %d deleted in %s",
                classes.size(), new Rel(this.placed)
            );
        } else if (deleted == classes.size()) {
            Logger.info(
                this, "All %d binari(es) deleted, which were found in %s",
                classes.size(), new Rel(this.placed)
            );
        } else {
            Logger.info(
                this, "Just %d binari(es) out of %d deleted in %s",
                deleted, classes.size(), new Rel(this.placed)
            );
        }
    }

    /**
     * Keep those we must keep selectively.
     * @param all All binaries found
     * @return Number of files deleted
     * @throws IOException If fails
     */
    private int killThem(final Iterable<PlacedTojo> all) throws IOException {
        int unplaced = 0;
        for (final PlacedTojo tojo : all) {
            final String related = tojo.related();
            final Path path = Paths.get(tojo.identifier());
            final String hash = new FileHash(path).toString();
            if (!tojo.sameHash(hash)) {
                if (hash.isEmpty()) {
                    Logger.debug(
                        this, "The binary %s of %s is gone, won't unplace",
                        related, tojo.dependency()
                    );
                    continue;
                }
                if (!UnplaceMojo.inside(related, this.removeBinaries)) {
                    Logger.warn(
                        this, "The binary %s of %s looks different, won't unplace",
                        related, tojo.dependency()
                    );
                    continue;
                }
                Logger.info(
                    this,
                    "The binary %s of %s looks different, but its unplacing is mandatory as 'mandatoryUnplace' option specifies",
                    related, tojo.dependency()
                );
            }
            if (UnplaceMojo.inside(related, this.keepBinaries)
                && !UnplaceMojo.inside(related, this.removeBinaries)) {
                continue;
            }
            if (UnplaceMojo.delete(path)) {
                unplaced += 1;
                tojo.unplace();
                Logger.debug(
                    this, "Binary %s of %s deleted",
                    new Rel(path), tojo.dependency()
                );
            } else {
                Logger.debug(
                    this, "Binary %s of %s already deleted",
                    new Rel(path), tojo.dependency()
                );
            }
        }
        return unplaced;
    }

    /**
     * Keep those we must keep selectively.
     * @param tojos All binaries found
     * @return Number of files deleted
     * @throws IOException If fails
     */
    private int keepThem(final Iterable<? extends PlacedTojo> tojos) throws IOException {
        int deleted = 0;
        int remained = 0;
        for (final PlacedTojo tojo : tojos) {
            final String related = tojo.related();
            final Path path = Paths.get(tojo.identifier());
            if (!this.keepBinaries.isEmpty()
                && UnplaceMojo.inside(related, this.keepBinaries)) {
                remained += 1;
                continue;
            }
            if (UnplaceMojo.delete(path)) {
                deleted += 1;
                Logger.debug(
                    this,
                    "The binary %s of %s is removed since it doesn't match 'selectivelyPlace' list of globs",
                    related, tojo.dependency()
                );
            } else {
                Logger.debug(
                    this, "Binary %s of %s already deleted",
                    new Rel(path), tojo.dependency()
                );
            }
        }
        Logger.info(
            this,
            "Because of 'selectivelyPlace' list of globs: %d files remained and %d deleted",
            remained, deleted
        );
        return deleted;
    }

    /**
     * This file is matched by one of the globs?
     * @param related The related name of the file
     * @param globs The globs
     * @return TRUE if inside this list of globx
     */
    private static boolean inside(final String related, final Iterable<String> globs) {
        return new ListOf<>(globs).stream().anyMatch(
            glob -> UnplaceMojo.matches(related, glob)
        );
    }

    /**
     * The file matches the glob?
     * @param related The related name of the file
     * @param glob The glob
     * @return TRUE if matches
     */
    private static boolean matches(final String related, final String glob) {
        return FileSystems.getDefault().getPathMatcher(
            String.format("glob:%s", glob)
        ).matches(Paths.get(related));
    }

    /**
     * Delete file and its parent if it's empty.
     * @param file The file
     * @return TRUE if deleted
     * @throws IOException If fails
     */
    private static boolean delete(final Path file) throws IOException {
        Path dir = file.getParent();
        boolean deleted = false;
        if (Files.exists(file)) {
            Files.delete(file);
            deleted = true;
        }
        while (UnplaceMojo.isEmpty(dir)) {
            final Path curdir = dir;
            dir = curdir.getParent();
            Files.delete(curdir);
            Logger.debug(
                UnplaceMojo.class,
                "Empty directory deleted too: %s",
                new Rel(dir)
            );
        }
        return deleted;
    }

    /**
     * Check if folder is empty.
     * @param path Folder to be checked
     * @return True if folder is empty and False otherwise
     * @throws IOException In case of I/O issues
     */
    private static boolean isEmpty(final Path path) throws IOException {
        boolean empty = false;
        if (Files.isDirectory(path)) {
            try (DirectoryStream<Path> directory = Files.newDirectoryStream(path)) {
                empty = !directory.iterator().hasNext();
            }
        }
        return empty;
    }
}
