/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

/**
 * BYTES.RIGHT.
 *
 * @since 0.1.0
 * @checkstyle TypeNameCheck (15 lines)
 */
@XmirObject(oname = "bytes.right")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOright extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EObytes$EOright() {
        this.add("x", new AtVoid("x"));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO))
                .asBytes()
                .shift(new Dataized(this.take("x")).asNumber().intValue())
                .take()
        );
    }
}
