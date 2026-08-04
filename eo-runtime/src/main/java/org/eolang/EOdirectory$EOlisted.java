/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

/**
 * Directory.listed.
 * @since 0.63
 * @checkstyle IllegalIdentifierNameCheck (101 lines)
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "directory.listed")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOdirectory$EOlisted extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOdirectory$EOlisted() {
        // nothing
    }

    @Override
    public Phi lambda() {
        final String raw = new Dataized(
            this.take(Phi.RHO).take("file").take("path")
        ).asString();
        final Path path;
        try {
            path = Paths.get(raw).toAbsolutePath();
        } catch (final InvalidPathException ex) {
            throw new ExFailure(
                String.format("'%s' is not a valid path", raw),
                ex
            );
        }
        try (Stream<Path> paths = Files.list(path)) {
            return new Data.ToPhi(
                paths
                    .map(p -> p.getFileName().toString())
                    .sorted()
                    .map(name -> (Phi) new Data.ToPhi(name))
                    .toArray(Phi[]::new)
            );
        } catch (final IOException ex) {
            throw new ExFailure(
                String.format("Can't list the directory '%s'", path),
                ex
            );
        }
    }
}
