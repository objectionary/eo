/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.regex.PatternSyntaxException;
import java.util.stream.Stream;

/**
 * Directory.walk.
 * @since 0.40
 * @checkstyle IllegalIdentifierNameCheck (101 lines)
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "directory.walk")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOdirectory$EOwalk extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOdirectory$EOwalk() {
        super(new Attrs(new Attr("glob", new AtVoid("glob"))));
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
                String.format(
                    "'%s' is not a valid path: %s", raw, ex.getReason()
                ),
                ex
            );
        }
        final String glob = new Dataized(this.take("glob")).asString();
        final PathMatcher matcher;
        try {
            matcher = FileSystems.getDefault().getPathMatcher(
                String.format("glob:%s", glob)
            );
        } catch (final PatternSyntaxException ex) {
            throw new ExFailure(
                String.format("Can't parse '%s' as a glob pattern", glob),
                ex
            );
        }
        try (Stream<Path> paths = Files.walk(path)) {
            return new Data.ToPhi(
                paths
                    .filter(p -> matcher.matches(path.relativize(p))).map(
                        p -> {
                            final Phi file = Phi.Φ.take("file").copy();
                            file.put(0, new ToPhi(p.toString()));
                            return file;
                        }
                    )
                    .toArray(Phi[]::new)
            );
        } catch (final IOException ex) {
            throw new ExFailure(
                String.format("Can't walk the directory '%s'", path),
                ex
            );
        }
    }
}
