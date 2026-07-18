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
 * Malloc.of.allocated.read object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "malloc.of.allocated.read")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOmalloc$EOof$EOallocated$EOread extends PhDefault implements Atom {

    /**
     * Name of the error-branch void that holds the caller's read fallback.
     */
    private static final String FALLBACK = "cantread";

    /**
     * Ctor.
     */
    public EOmalloc$EOof$EOallocated$EOread() {
        super(new Attrs(
            new Attr("offset", new AtVoid("offset")),
            new Attr("length", new AtVoid("length")),
            new Attr(
                EOmalloc$EOof$EOallocated$EOread.FALLBACK,
                new AtVoid(EOmalloc$EOof$EOallocated$EOread.FALLBACK)
            )
        ));
    }

    @Override
    public Phi lambda() {
        return this.bytes(
            new Expect.Natural(Expect.at(this.take(Phi.RHO), "id")).it(),
            new Expect.Natural(Expect.at(this, "offset")).it(),
            new Expect.Natural(Expect.at(this, "length")).it()
        );
    }

    /**
     * Read the bytes from the allocated block, or fall back when the
     * requested range lies outside it (R-3.4.8 error-branch semantics).
     * The predictable out-of-range case is asked of {@link Heaps#fits}
     * up front — not caught as a failure — so it routes cleanly to the
     * caller-supplied {@code cant-read} fallback (the bottom object ⊥
     * when none was provided). A non-allocated block is unpredictable
     * and {@code fits} aborts on it, which the atom does not handle.
     * @param id Block identifier
     * @param offset Offset to read from
     * @param length Number of bytes to read
     * @return The bytes read, or the {@code cant-read} fallback
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private Phi bytes(final int id, final int offset, final int length) {
        final Phi result;
        if (Heaps.INSTANCE.fits(id, offset, length)) {
            result = new Data.ToPhi(Heaps.INSTANCE.read(id, offset, length));
        } else {
            result = this.take(EOmalloc$EOof$EOallocated$EOread.FALLBACK);
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
