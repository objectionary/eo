/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * BYTES.CONCAT.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.concat")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOconcat extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EObytes$EOconcat() {
        this.add("b", new AtVoid("b"));
    }

    @Override
    public Phi lambda() {
        final byte[] current = new Dataized(this.take(Attr.RHO)).take();
        final byte[] provided = new Dataized(this.take("b")).take();
        final byte[] dest = new byte[current.length + provided.length];
        System.arraycopy(current, 0, dest, 0, current.length);
        System.arraycopy(provided, 0, dest, current.length, provided.length);
        return new Data.ToPhi(dest);
    }
}
