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
     * Ctor.
     */
    public EOmalloc$EOof$EOallocated$EOread() {
        super(new Attrs(
            new Attr("offset", new AtVoid("offset")),
            new Attr("length", new AtVoid("length")),
            new Attr("cant-read", new AtVoid("cant-read"))
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
     * The range is checked against the block size up front rather than
     * by catching a failure, so an out-of-range read routes cleanly to
     * the caller-supplied {@code cant-read} fallback — the bottom object
     * (⊥) when no fallback was provided.
     * @param id Block identifier
     * @param offset Offset to read from
     * @param length Number of bytes to read
     * @return The bytes read, or the {@code cant-read} fallback
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private Phi bytes(final int id, final int offset, final int length) {
        final Phi result;
        if (offset + length <= Heaps.INSTANCE.size(id)) {
            result = new Data.ToPhi(Heaps.INSTANCE.read(id, offset, length));
        } else {
            result = this.take("cant-read");
        }
        return result;
    }
}
