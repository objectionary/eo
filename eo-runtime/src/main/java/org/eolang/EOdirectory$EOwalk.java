/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.stream.Stream;

/**
 * Directory.walk.
 * @since 0.40
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
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public Phi lambda() {
        final Path path = Paths.get(
            new Dataized(
                this.take(Phi.RHO).take("file").take("path")
            ).asString()
        ).toAbsolutePath();
        final PathMatcher matcher = FileSystems.getDefault().getPathMatcher(
            String.format(
                "glob:%s", new Dataized(
                    this.take("glob")
                ).asString()
            )
        );
        try (Stream<Path> paths = Files.walk(path)) {
            return new Data.ToPhi(
                paths
                    .map(p -> p.toAbsolutePath().toString())
                    .map(p -> p.substring(p.indexOf(path.toString())))
                    .filter(p -> matcher.matches(Paths.get(p))).map(
                        p -> {
                            final Phi file = Phi.Φ.take("file").copy();
                            file.put(0, new ToPhi(p));
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
