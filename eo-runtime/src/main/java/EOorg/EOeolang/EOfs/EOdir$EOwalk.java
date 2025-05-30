/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOfs; // NOPMD

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.stream.Stream;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Dir.walk.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "dir.walk")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOdir$EOwalk extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOdir$EOwalk() {
        this.add("glob", new PhVoid("glob"));
    }

    @Override
    public Phi lambda() {
        final Path path = Paths.get(
            new Dataized(
                this.take(Phi.RHO).take("file").take("path")
            ).asString()
        ).toAbsolutePath();
        final String glob = new Dataized(
            this.take("glob")
        ).asString();
        final PathMatcher matcher = FileSystems.getDefault().getPathMatcher(
            String.format("glob:%s", glob)
        );
        try (Stream<Path> paths = Files.walk(path)) {
            return new Data.ToPhi(
                paths
                    .map(p -> p.toAbsolutePath().toString())
                    .map(p -> p.substring(p.indexOf(path.toString())))
                    .filter(p -> matcher.matches(Paths.get(p)))
                    .map(
                        p -> {
                            final Phi file = Phi.Φ.take("org.eolang.fs.file").copy();
                            file.put(0, new ToPhi(p));
                            return file;
                        }
                    )
                    .toArray(Phi[]::new)
            );
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't walk at %s", path),
                ex
            );
        }
    }
}
