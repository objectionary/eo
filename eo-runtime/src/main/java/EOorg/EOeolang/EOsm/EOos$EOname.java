/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm; // NOPMD

import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Os.name.
 * @since 0.40
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "os.name")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOos$EOname extends PhDefault implements Atom {
    /**
     * Operating system name as {@link Phi}.
     */
    private static final Phi OS_NAME = new Data.ToPhi(System.getProperty("os.name"));

    @Override
    public Phi lambda() {
        return EOos$EOname.OS_NAME;
    }
}
