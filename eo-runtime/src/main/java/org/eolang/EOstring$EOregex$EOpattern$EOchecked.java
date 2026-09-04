/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Regex.pattern.checked object.
 *
 * <p>Hands the pattern back when its {@code source} compiles, and otherwise
 * applies the {@code cant-check} fallback to the construct the engine choked
 * on and to the index it sits at in that source, both as the engine reports
 * them (an index of {@code -1} when it has none). How the source is spliced
 * together from the body and the flags, how the index is turned into an
 * offset into the body the caller wrote, and what the message reads like are
 * all decided in {@code regex.eo}, so this atom knows nothing about slashes,
 * flags or wording.</p>
 *
 * @since 0.77
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "string.regex.pattern.checked")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOstring$EOregex$EOpattern$EOchecked extends PhDefault implements Atom {

    /**
     * Name of the error-branch void that holds the caller's fallback.
     */
    private static final String FALLBACK = "cant-check";

    /**
     * Ctor.
     */
    public EOstring$EOregex$EOpattern$EOchecked() {
        super(
            new Attrs(
                new Attr(Phi.RHO, new AtRho()),
                new Attr(
                    EOstring$EOregex$EOpattern$EOchecked.FALLBACK,
                    new AtVoid(EOstring$EOregex$EOpattern$EOchecked.FALLBACK)
                )
            )
        );
    }

    @Override
    public Phi lambda() {
        final Phi pattern = this.take(Phi.RHO);
        Phi result = pattern;
        try {
            Pattern.compile(new Dataized(pattern.take("source")).asString());
        } catch (final PatternSyntaxException ex) {
            result = this.take(EOstring$EOregex$EOpattern$EOchecked.FALLBACK);
            result.put(0, new Data.ToPhi(ex.getDescription()));
            result.put(1, new Data.ToPhi(ex.getIndex()));
        }
        return result;
    }
}
