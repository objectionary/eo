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
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * File.open.process-file.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "file.open.process-file")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOfile$EOopen$EOprocess_file extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Phi open = this.take(Attr.RHO);
        final Path path = Paths.get(
            new Dataized(open.take(Attr.RHO).take("path")).asString()
        );
        try {
            Files.INSTANCE.open(path.toString());
            try {
                final Phi scope = open.take("scope").copy();
                scope.put(0, open.take("file-stream"));
                new Dataized(scope).take();
            } finally {
                Files.INSTANCE.close(path.toString());
            }
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't process file %s", path),
                ex
            );
        }
        return new Data.ToPhi(true);
    }
}
