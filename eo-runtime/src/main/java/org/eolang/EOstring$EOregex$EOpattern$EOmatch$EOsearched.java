/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Regex.pattern.match.searched object.
 *
 * <p>Searches the text of the enclosing {@code match} for the first block
 * its pattern matches at or after {@code start}. Positions are counted in
 * characters rather than in UTF-16 units, so a supplementary character is
 * one position on both sides of the boundary. The answer is a tuple: empty
 * when nothing matches, and otherwise the {@code from} and {@code to}
 * positions of the block, a tuple of its groups (group zero first, an empty
 * string for a group that did not participate) and a tuple telling which of
 * those groups did participate. Turning that tuple into a matched block, or
 * into the missing one, is done in {@code regex.eo}.</p>
 *
 * @since 0.77
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "string.regex.pattern.match.searched")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOstring$EOregex$EOpattern$EOmatch$EOsearched extends PhDefault
    implements Atom {

    /**
     * Name of the void that holds the position to search from.
     */
    private static final String START = "start";

    /**
     * Ctor.
     */
    public EOstring$EOregex$EOpattern$EOmatch$EOsearched() {
        super(
            new Attrs(
                new Attr(Phi.RHO, new AtRho()),
                new Attr(
                    EOstring$EOregex$EOpattern$EOmatch$EOsearched.START,
                    new AtVoid(EOstring$EOregex$EOpattern$EOmatch$EOsearched.START)
                )
            )
        );
    }

    @Override
    public Phi lambda() {
        final Phi match = this.take(Phi.RHO);
        final String text = new Dataized(match.take("txt")).asString();
        final String source = new Dataized(match.take(Phi.RHO).take("source")).asString();
        final int start = new Natural(
            Expect.at(this, EOstring$EOregex$EOpattern$EOmatch$EOsearched.START)
        ).it();
        final int length = text.codePointCount(0, text.length());
        if (start > length) {
            throw new ExFailure(
                "the 'start' attribute (%d) must be less than or equal to text length (%d)",
                start,
                length
            );
        }
        final Matcher matcher;
        try {
            matcher = Pattern.compile(source).matcher(text);
        } catch (final PatternSyntaxException ex) {
            throw new ExFailure(
                String.format("cannot search with the regex pattern '%s'", source),
                ex
            );
        }
        final Phi[] block;
        if (matcher.find(text.offsetByCodePoints(0, start))) {
            block = EOstring$EOregex$EOpattern$EOmatch$EOsearched.block(matcher, text);
        } else {
            block = new Phi[0];
        }
        return new Data.ToPhi(block);
    }

    private static Phi[] block(final Matcher matcher, final String text) {
        final Phi[] groups = new Phi[matcher.groupCount() + 1];
        final Phi[] existing = new Phi[groups.length];
        for (int idx = 0; idx < groups.length; ++idx) {
            final String captured = matcher.group(idx);
            groups[idx] = new Data.ToPhi(Optional.ofNullable(captured).orElse(""));
            existing[idx] = new Data.ToPhi(captured != null);
        }
        return new Phi[]{
            new Data.ToPhi(text.codePointCount(0, matcher.start())),
            new Data.ToPhi(text.codePointCount(0, matcher.end())),
            new Data.ToPhi(groups),
            new Data.ToPhi(existing),
        };
    }
}
