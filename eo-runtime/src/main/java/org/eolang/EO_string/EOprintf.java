/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.util.Locale;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Attrs;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Printf.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "printf")
public final class EOprintf extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOprintf() {
        super(new Attrs(
            new Attr("format", new AtVoid("format")),
            new Attr("args", new AtVoid("args"))
        ));
    }

    @Override
    public Phi lambda() {
        final String format = new Dataized(this.take("format")).asString();
        return new Data.ToPhi(
            String.format(
                Locale.US,
                PrintfArgs.javaFormat(format),
                new PrintfArgs(
                    format,
                    Expect.at(this, "args")
                        .that(phi -> new Dataized(phi.take("length")).asNumber().intValue())
                        .otherwise("be a tuple with the 'length' attribute")
                        .it(),
                    Expect.at(this, "args")
                        .that(phi -> phi.take("at"))
                        .otherwise("be a tuple with the 'at' attribute")
                        .it()
                ).formatted().toArray()
            )
        );
    }
}
