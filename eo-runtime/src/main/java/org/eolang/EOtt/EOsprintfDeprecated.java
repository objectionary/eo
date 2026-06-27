/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EOtt; // NOPMD

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
 * Sprintf.
 * @since 0.39.0
 * @deprecated Use the pure-EO {@code tt.sprintf} object instead.
 * @todo #4752:30min Remove this deprecated Java atom together with {@link SprintfArgs}
 *  and {@code SprintfArgsTest}. It was superseded by the pure-EO implementation of
 *  {@code sprintf} in {@code eo-runtime/src/main/eo/tt/sprintf.eo} and is kept here only
 *  to avoid a duplicate-class clash with the transpiler-generated {@code EOsprintf}
 *  during the transition. Delete it once the pure-EO version is merged.
 * @checkstyle TypeNameCheck (5 lines)
 */
@Deprecated
@XmirObject(oname = "sprintf")
public final class EOsprintfDeprecated extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOsprintfDeprecated() {
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
                format.replaceAll("%\\d+\\$([a-zA-Z])", "%s").replaceAll("%x", "%s"),
                new SprintfArgs(
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
