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
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * File.touched.touch.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "file.touched.touch")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOfile$EOtouched$EOtouch extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Path path = Paths.get(
            new Dataized(
                this.take(Attr.RHO).take(Attr.RHO).take("path")
            ).asString()
        );
        try {
            return new ToPhi(
                path.toFile().createNewFile()
            );
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't touch file %s", path),
                ex
            );
        }
    }
}
