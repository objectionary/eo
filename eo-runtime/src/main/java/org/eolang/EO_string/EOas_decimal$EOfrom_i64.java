/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Attrs;
import org.eolang.BytesOf;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * String.as-decimal.from-i64 object.
 * @since 0.74.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "as-decimal.from-i64")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOas_decimal$EOfrom_i64 extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOas_decimal$EOfrom_i64() {
        super(new Attrs(new Attr("value", new AtVoid("value"))));
    }

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public Phi lambda() {
        final long value = new Dataized(this.take("value")).take(Long.class);
        return new Data.ToPhi(new BytesOf(Long.toString(value)).take());
    }
}
