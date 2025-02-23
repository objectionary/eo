/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Number.floor object.
 *
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.floor")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOfloor extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Expect.Number(Expect.at(this, Attr.RHO)).it().longValue()
        );
    }
}
