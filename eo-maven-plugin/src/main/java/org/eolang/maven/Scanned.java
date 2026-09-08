/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import org.codehaus.plexus.util.DirectoryScanner;

/**
 * Regular files under a directory, found by the scan Maven itself runs.
 *
 * <p>A directory a version control system owns, such as {@code .git} or
 * {@code .svn}, is pruned while the scan is under way, instead of being
 * walked whole and thrown away afterwards, and so are the backup and
 * editor droppings the scan excludes by default.</p>
 *
 * <p>Only regular files come out. A directory is not one, and neither is a
 * FIFO, a socket, a device node or a link with nothing at the end of it,
 * and a goal handed such an entry cannot make an EO program out of it: it
 * would hash and read it, and reading a FIFO waits for a writer that never
 * comes. A link to an ordinary file is still found, since such a source
 * reads exactly like the file it names, while a link to a directory is not
 * descended into, because the files behind it carry their own names.</p>
 *
 * <p>A directory that is not there holds no files, which is what a goal
 * running before the one that creates it has to see.</p>
 *
 * @since 0.73.4
 */
final class Scanned implements Iterable<Path> {

    /**
     * The home.
     */
    private final Path home;

    /**
     * Ctor.
     * @param dir The directory
     */
    Scanned(final Path dir) {
        this.home = dir;
    }

    @Override
    public Iterator<Path> iterator() {
        final Collection<Path> files = new ArrayList<>(0);
        if (Files.exists(this.home)) {
            files.addAll(this.found());
        }
        return files.iterator();
    }

    private Collection<Path> found() {
        final DirectoryScanner scanner = new DirectoryScanner();
        scanner.setBasedir(this.home.toFile());
        scanner.setIncludes(new String[] {"**"});
        scanner.setFollowSymlinks(false);
        scanner.addDefaultExcludes();
        scanner.scan();
        final Collection<Path> files = new ArrayList<>(0);
        for (final String name : scanner.getIncludedFiles()) {
            files.add(this.home.resolve(name));
        }
        return files;
    }
}
