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
 * File.moved.move.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "file.moved.move")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOfile$EOmoved$EOmove extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Phi rho = this.take(Phi.RHO);
        final Path target = Paths.get(
            new Dataized(rho.take("target")).asString()
        );
        final Path from = Paths.get(
            new Dataized(rho.take(Phi.RHO).take("path")).asString()
        );
        try {
            Files.move(from, target);
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't move %s to %s", from, target),
                ex
            );
        }
        return new Data.ToPhi(target.toString());
    }
}
