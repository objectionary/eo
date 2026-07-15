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
 * RECOVERED.
 *
 * <p>Resolves {@code value} to its normal form; if that is a terminated
 * computation (⊥), behaves as {@code alternative}, otherwise as {@code value}.
 * A ⊥ on both sides stays a ⊥, so an outer recovery can still intercept it.
 * This is the only way to intercept a ⊥ and keep going.</p>
 *
 * @since 0.74.0
 * @checkstyle TypeNameCheck (5 lines)
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
        final Phi picked = this.take("value").normalized();
        final Phi result;
        if (picked instanceof PhTerminator) {
            result = this.take("alternative").normalized();
        } else {
            result = picked;
        }
        return result;
    }
}
