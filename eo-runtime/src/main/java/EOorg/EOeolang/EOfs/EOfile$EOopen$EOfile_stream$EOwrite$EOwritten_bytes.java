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
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * File.open.file-stream.write.written-bytes.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "file.open.file-stream.write.written-bytes")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOfile$EOopen$EOfile_stream$EOwrite$EOwritten_bytes
    extends PhDefault
    implements Atom {

    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOfile$EOopen$EOfile_stream$EOwrite$EOwritten_bytes() {
        this.add("buffer", new PhVoid("buffer"));
    }

    @Override
    public Phi lambda() {
        final Path path = Paths.get(
            new Dataized(
                this.take(Attr.RHO)
                    .take(Attr.RHO)
                    .take(Attr.RHO)
                    .take(Attr.RHO)
                    .take("path")
            ).asString()
        );
        try {
            Files.INSTANCE.write(
                path.toString(),
                new Dataized(this.take("buffer")).take()
            );
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't write to %s", path),
                ex
            );
        }
        return new Data.ToPhi(true);
    }
}
