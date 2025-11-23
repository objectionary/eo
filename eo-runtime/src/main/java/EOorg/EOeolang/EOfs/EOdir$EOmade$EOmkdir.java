/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOfs; // NOPMD

import java.nio.file.Paths;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Dir.made.mkdir.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "dir.made.mkdir")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOdir$EOmade$EOmkdir extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Paths.get(
                new Dataized(
                    this.take(Phi.RHO).take(Phi.RHO).take("file").take("path")
                ).asString()
            ).toFile().mkdirs()
        );
    }
}
