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
 * BYTES.CONCAT.
 * @since 0.23
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.concat")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOconcat extends PhDefault implements Atom {

    /**
     * Name of the error-branch void that holds the caller's concat fallback.
     */
    private static final String FALLBACK = "cant-concat";

    /**
     * Ctor.
     */
    public EObytes$EOconcat() {
        super(new Attrs(
            new Attr("b", new AtVoid("b")),
            new Attr(EObytes$EOconcat.FALLBACK, new AtVoid(EObytes$EOconcat.FALLBACK))
        ));
    }

    @Override
    public Phi lambda() {
        final byte[] current = new Dataized(this.take(Phi.RHO)).take();
        final byte[] provided = new Dataized(this.take("b")).take();
        final Phi result;
        if ((long) current.length + provided.length <= Integer.MAX_VALUE) {
            final byte[] dest = new byte[current.length + provided.length];
            System.arraycopy(current, 0, dest, 0, current.length);
            System.arraycopy(provided, 0, dest, current.length, provided.length);
            result = new Data.ToPhi(dest);
        } else {
            result = this.take(EObytes$EOconcat.FALLBACK);
            result.put(
                0,
                new Data.ToPhi(
                    String.format(
                        "cannot concatenate bytes of size %d with bytes of size %d",
                        current.length, provided.length
                    )
                )
            );
        }
        return result;
    }
}
