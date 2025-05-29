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
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * File.open.file-stream.read.read-bytes.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "file.open.file-stream.read.read-bytes")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOfile$EOopen$EOfile_stream$EOread$EOread_bytes
    extends PhDefault
    implements Atom {

    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOfile$EOopen$EOfile_stream$EOread$EOread_bytes() {
        this.add("size", new PhVoid("size"));
    }

    @Override
    public Phi lambda() {
        final Path path = Paths.get(
            new Dataized(
                this.take(Phi.RHO)
                    .take(Phi.RHO)
                    .take(Phi.RHO)
                    .take(Phi.RHO)
                    .take("path")
            ).asString()
        );
        try {
            return new ToPhi(
                Files.INSTANCE.read(
                    path.toString(),
                    new Dataized(this.take("size")).asNumber().intValue()
                )
            );
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't read from %s", path),
                ex
            );
        }
    }
}
