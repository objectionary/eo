/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * RECOVERED.
 *
 * <p>Resolves {@code value} to its normal form; if that is a terminated
 * computation (a terminator), behaves as {@code alternative}, otherwise as {@code value}.
 * A terminator on both sides stays a terminator, so an outer recovery can still
 * intercept it. This is the only way to intercept a terminator and keep going.</p>
 *
 * <p>A terminator arrives in two shapes. It is a {@link PhTerminator} when nothing
 * forced it on the way here, and it is an {@link ExAbstract} when something did:
 * a {@code seq} step, a const, or anything else that dataizes while the normal
 * form is computed. Both are the same termination, so both are intercepted.</p>
 *
 * <p>Any {@link ExAbstract} is intercepted and not only an {@link ExFailure},
 * because an {@link ExReadOnly} is just as much an EO-level termination, and
 * whether it arrives wrapped into an {@link ExFailure} depends on the
 * {@code trackLocations} flag the program was compiled with. The two that are
 * not terminations pass through, the same two {@link PhSafe} lets through: an
 * {@link ExInterrupted} stops the thread and an {@link ExAgain} hands the next
 * iteration to a {@link PhLoop}.</p>
 *
 * @since 0.64.0
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
        } catch (final ExInterrupted | ExAgain ex) {
            throw ex;
        } catch (final ExAbstract ex) {
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
