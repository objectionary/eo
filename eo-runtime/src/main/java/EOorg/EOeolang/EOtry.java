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
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * TRY.
 *
 * @since 0.19
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "try")
public final class EOtry extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOtry() {
        this.add("main", new AtVoid("main"));
        this.add("catch", new AtVoid("catch"));
        this.add("finally", new AtVoid("finally"));
    }

    @Override
    public Phi lambda() {
        byte[] result;
        try {
            result = new Dataized(this.take("main")).take();
        } catch (final EOerror.ExError ex) {
            final Phi catcher = this.take("catch").copy();
            catcher.put(0, ex.enclosure());
            result = new Dataized(catcher).take();
        } finally {
            new Dataized(this.take("finally")).take();
        }
        return new Data.ToPhi(result);
    }
}
