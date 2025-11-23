/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOfs; // NOPMD

import java.nio.file.Files;
import java.nio.file.Paths;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * File.exists.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "file.exists")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOfile$EOexists extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Files.exists(
                Paths.get(
                    new Dataized(
                        this.take(Attr.RHO).take("path")
                    ).asString()
                )
            )
        );
    }
}
