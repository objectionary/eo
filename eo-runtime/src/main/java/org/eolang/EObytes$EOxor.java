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
 * BYTES.XOR.
 *
 * @since 0.1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.xor")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOxor extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EObytes$EOxor() {
        this.add("b", new AtVoid("b"));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO)).asBytes().xor(
                new Dataized(this.take("b")).asBytes()
            ).take()
        );
    }
}
