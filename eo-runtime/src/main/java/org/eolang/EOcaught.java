/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * CAUGHT.
 *
 * <p>Resolves {@code value} to its normal form; if that is a terminated
 * computation, behaves as {@code alternative} applied to the message the
 * termination carried, otherwise as {@code value}. This is the only way to
 * read that message, which {@link EOrecovered} drops.</p>
 *
 * <p>The message is handed over by position, the way an atom hands one to a
 * fallback, so an {@code alternative} with no void to take it aborts instead
 * of losing the message again.</p>
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
        final Phi picked = new Resolved(this.take("value")).it();
        final Phi result;
        if (picked instanceof final PhTerminator terminator) {
            result = this.take("alternative");
            result.put(0, terminator.reason());
        } else {
            result = picked;
        }
        return result;
    }
}
