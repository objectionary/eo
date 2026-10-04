/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * CAUGHT.
 *
 * <p>Resolves {@code value} to its normal form; if that is a terminated
 * computation (a terminator), behaves as {@code alternative} applied to the
 * message the termination carried, otherwise as {@code value}. This is the
 * only way to read that message, which {@link EOrecovered} drops.</p>
 *
 * <p>The message is handed over by position, the way an atom hands one to a
 * fallback, so {@code alternative} must have a void to take it. One that has
 * none aborts, instead of losing the message the way the alternative of
 * {@link EOrecovered} does.</p>
 *
 * <p>A terminator arrives in two shapes. It is a {@link PhTerminator} when
 * nothing forced it on the way here, and it is an {@link ExFailure} when
 * something did: a {@code seq} step, a const, or anything else that dataizes
 * while the normal form is computed. Both are the same termination, so both
 * are intercepted and both carry a message.</p>
 *
 * @since 0.0.0
 */
@XmirObject(oname = "caught")
public final class EOcaught extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOcaught() {
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
            result = this.take("alternative");
            result.put(0, ((PhTerminator) picked).reason());
        } else {
            result = picked;
        }
        return result;
    }
}
