/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Chunk.read object.
 *
 * @since 0.36.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "chunk.read")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOchunk$EOread extends PhDefault implements Atom {

    /**
     * Name of the error-branch void that holds the caller's read fallback.
     */
    private static final String FALLBACK = "cant-read";

    /**
     * Ctor.
     */
    public EOchunk$EOread() {
        super(new Attrs(
            new Attr(Phi.RHO, new AtRho()),
            new Attr("offset", new AtVoid("offset")),
            new Attr("length", new AtVoid("length")),
            new Attr(
                EOchunk$EOread.FALLBACK,
                new AtVoid(EOchunk$EOread.FALLBACK)
            )
        ));
    }

    @Override
    public Phi lambda() {
        return this.bytes(
            new Natural(Expect.at(this.take(Phi.RHO), "id")).it(),
            new Natural(Expect.at(this, "offset")).it(),
            new Natural(Expect.at(this, "length")).it()
        );
    }

    private Phi bytes(final int id, final int offset, final int length) {
        final Phi result;
        if (Heaps.INSTANCE.fits(id, offset, length)) {
            result = new Data.ToPhi(Heaps.INSTANCE.read(id, offset, length));
        } else {
            result = this.take(EOchunk$EOread.FALLBACK);
            result.put(
                0,
                new Data.ToPhi(
                    String.format(
                        "Can't read '%d' bytes from offset '%d', because only '%d' are allocated",
                        length,
                        offset,
                        Heaps.INSTANCE.size(id)
                    )
                )
            );
        }
        return result;
    }
}
