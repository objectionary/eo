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
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Dir.tmpfile.touch.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "dir.tmpfile.touch")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOdir$EOtmpfile$EOtouch extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Path home = Paths.get(
            new Dataized(
                this.take(Attr.RHO).take(Attr.RHO).take("path")
            ).asString()
        );
        final Path path;
        try {
            path = Files.createTempFile(home, null, null);
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't created temp file in %s", home),
                ex
            );
        }
        path.toFile().deleteOnExit();
        return new Data.ToPhi(path.toAbsolutePath().toString());
    }
}
