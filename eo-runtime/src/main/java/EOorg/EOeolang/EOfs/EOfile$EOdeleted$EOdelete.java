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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * File.deleted.delete.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "file.deleted.delete")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOfile$EOdeleted$EOdelete extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Path path = Paths.get(
            new Dataized(
                this.take(Phi.RHO).take(Phi.RHO).take("path")
            ).asString()
        );
        try {
            Files.delete(path);
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't deleted %s", path),
                ex
            );
        }
        return new Data.ToPhi(true);
    }
}
