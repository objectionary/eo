/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.util.IllegalFormatException;
import java.util.Locale;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Attrs;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
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
        try {
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
                            .that(phi -> phi)
                            .otherwise("be a tuple")
                            .it()
                    ).formatted().toArray()
                )
            );
        } catch (final IllegalFormatException ex) {
            throw new ExFailure(
                String.format("The format '%s' is not a valid printf format string", format),
                ex
            );
        }
    }
}
