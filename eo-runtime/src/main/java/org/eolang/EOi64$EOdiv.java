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
 * The i64.div object.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "i64.div")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOi64$EOdiv extends PhDefault implements Atom {

    /**
     * Name of the error-branch void that holds the caller's divide-by-zero fallback.
     */
    private static final String FALLBACK = "cant-divide";

    /**
     * Ctor.
     */
    public EOi64$EOdiv() {
        super(new Attrs(
            new Attr("x", new AtVoid("x")),
            new Attr(EOi64$EOdiv.FALLBACK, new AtVoid(EOi64$EOdiv.FALLBACK))
        ));
    }

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public Phi lambda() {
        final long dividend = new Expect.I64(Expect.at(this, Phi.RHO)).it();
        final long divisor = new Expect.I64(Expect.at(this, "x")).it();
        final Phi result;
        if (divisor == 0L) {
            result = this.take(EOi64$EOdiv.FALLBACK);
            result.put(0, new Data.ToPhi("i64 cannot be divided by zero"));
        } else {
            result = Phi.Φ.take("i64").copy();
            result.put(0, new Data.ToPhi(new BytesOf(dividend / divisor).take()));
        }
        return result;
    }
}
