/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * WHILE.
 * Makes {@code body} for the indexes 0, 1, 2 and so on, while {@code condition}
 * says yes about the next index, dataizes all of them but the last one, and
 * gives the last one back, or {@code false} when the first index was refused.
 * The cycles do not nest.
 * @since 0.74.0
 */
@XmirObject(oname = "while")
public final class EOwhile extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwhile() {
        super(new Attrs(
            new Attr("condition", new AtVoid("condition")),
            new Attr("body", new AtVoid("body"))
        ));
    }

    @Override
    public Phi lambda() {
        final Phi last;
        if (this.holds(0L)) {
            long index = 0L;
            Phi current = this.applied("body", index);
            while (this.holds(index + 1L)) {
                new Dataized(current).take();
                index += 1L;
                current = this.applied("body", index);
            }
            last = current;
        } else {
            last = new Data.ToPhi(false);
        }
        return last;
    }

    /**
     * Does the condition hold for this index?
     * @param index The index of the cycle
     * @return TRUE if the cycle has to be made
     */
    private boolean holds(final long index) {
        return new Dataized(this.applied("condition", index)).asBool();
    }

    /**
     * Copy of the attribute with the index applied to it, not dataized.
     * @param attr The name of the attribute
     * @param index The index of the cycle
     * @return The copy
     */
    private Phi applied(final String attr, final long index) {
        final Phi copy = this.take(attr).copy();
        copy.put(0, new Data.ToPhi(index));
        return copy;
    }
}
