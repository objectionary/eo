/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOtxt; // NOPMD

import java.util.Locale;
import org.eolang.Atom;
import org.eolang.Dataized;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sprintf.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "sprintf")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOsprintf extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOsprintf() {
        this.add("format", new PhVoid("format"));
        this.add("args", new PhVoid("args"));
    }

    @Override
    public Phi lambda() {
        final String format = new Dataized(this.take("format")).asString();
        final Phi retriever = Expect.at(this, "args")
            .that(phi -> phi.take("at"))
            .otherwise("be a tuple with the 'at' attribute")
            .it();
        final long length = Expect.at(this, "args")
            .that(phi -> new Dataized(phi.take("length")).asNumber().intValue())
            .otherwise("be a tuple with the 'length' attribute")
            .it();
        return new ToPhi(
            String.format(
                Locale.US,
                format.replaceAll("%x", "%s"),
                new SprintfArgs(format, length, retriever).formatted().toArray()
            )
        );
    }
}
