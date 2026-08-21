/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * RECOVERED.
 *
 * <p>Resolves {@code value} to its normal form; if that is a terminated
 * computation (bottom), behaves as {@code alternative}, otherwise as {@code value}.
 * A bottom on both sides stays a bottom, so an outer recovery can still intercept it.
 * This is the only way to intercept a bottom and keep going.</p>
 *
 * <p>A bottom arrives in two shapes. It is a {@link PhTerminator} when nothing
 * forced it on the way here, and it is an {@link ExFailure} when something did:
 * a {@code seq} step, a const, or anything else that dataizes while the normal
 * form is computed. Both are the same termination, so both are intercepted.</p>
 *
 * @since 0.74.0
 */
@XmirObject(oname = "recovered")
public final class EOrecovered extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOrecovered() {
        super(new Attrs(
            new Attr("value", new AtVoid("value")),
            new Attr("alternative", new AtVoid("alternative"))
        ));
    }

    @Override
    public Phi lambda() {
        Phi picked;
        try {
            picked = this.take("value").normalized();
        } catch (final ExFailure ex) {
            picked = new PhTerminator(new Data.ToPhi(ex.getMessage()));
        }
        final Phi result;
        if (picked instanceof PhTerminator) {
            result = this.take("alternative").normalized();
        } else {
            result = picked;
        }
        return result;
    }
}
