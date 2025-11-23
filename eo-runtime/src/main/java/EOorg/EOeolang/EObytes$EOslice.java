/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.util.Arrays;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * BYTES.SLICE.
 *
 * @since 0.1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.slice")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOslice extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EObytes$EOslice() {
        this.add("start", new PhVoid("start"));
        this.add("len", new PhVoid("len"));
    }

    @Override
    public Phi lambda() {
        final int start = Expect.at(this, "start")
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .must(number -> number % 1 == 0)
            .that(Double::intValue)
            .otherwise("must be an integer")
            .must(integer -> integer >= 0)
            .otherwise("must be a positive integer")
            .it();
        final int length = Expect.at(this, "len")
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .must(number -> number % 1 == 0)
            .that(Double::intValue)
            .otherwise("must be an integer")
            .must(integer -> integer >= 0)
            .otherwise("must be a positive integer")
            .it();
        return new Data.ToPhi(
            Arrays.copyOfRange(
                new Dataized(this.take(Attr.RHO)).take(),
                start, start + length
            )
        );
    }
}
