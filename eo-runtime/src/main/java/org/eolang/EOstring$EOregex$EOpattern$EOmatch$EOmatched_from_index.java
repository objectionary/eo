/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Regex.pattern.match.matched-from-index.
 * @since 0.39.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "string.regex.pattern.match.matched-from-index")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index extends PhDefault
    implements Atom {

    /**
     * Start.
     */
    private static final String START = "start";

    /**
     * Position.
     */
    private static final String POSITION = "position";

    /**
     * Ctor.
     */
    public EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index() {
        super(
            new Attrs(
                new Attr(Phi.RHO, new AtRho()),
                new Attr(
                    EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION,
                    new AtVoid(EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION)
                ),
                new Attr(
                    EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.START,
                    new AtVoid(EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.START)
                )
            )
        );
    }

    @Override
    public Phi lambda() {
        final Phi match = this.take(Phi.RHO);
        final Matcher matcher;
        final String text;
        try {
            text = new Dataized(match.take("txt")).asString();
            matcher = ((Pattern) new ObjectInputStream(
                new ByteArrayInputStream(
                    new Dataized(match.take(Phi.RHO).take("serialized")).take()
                )
            ).readObject()).matcher(text);
        } catch (final IOException | ClassNotFoundException | ClassCastException ex) {
            throw new ExFailure("cannot deserialize the compiled regex pattern", ex);
        }
        final int start = new Natural(
            Expect.at(this, EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.START)
        ).it();
        final int length = text.codePointCount(0, text.length());
        if (start > length) {
            throw new ExFailure(
                "the 'start' attribute (%d) must be less than or equal to text length (%d)",
                start,
                length
            );
        }
        final boolean found = matcher.find(text.offsetByCodePoints(0, start));
        final Phi result = match.take("matched");
        if (found) {
            this.fill(result, matcher, text);
        } else {
            this.blank(result);
        }
        return result;
    }

    private void fill(final Phi result, final Matcher matcher, final String text) {
        result.put(
            EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION,
            this.take(EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION)
        );
        result.put(
            EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.START,
            this.take(EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.START)
        );
        result.put("from", new Data.ToPhi(text.codePointCount(0, matcher.start())));
        result.put("to", new Data.ToPhi(text.codePointCount(0, matcher.end())));
        final Phi[] groups;
        final Phi[] exist;
        if (matcher.groupCount() > 0) {
            groups = new Phi[matcher.groupCount() + 1];
            exist = new Phi[matcher.groupCount() + 1];
            for (int idx = 0; idx < groups.length; ++idx) {
                final String captured = matcher.group(idx);
                groups[idx] = new Data.ToPhi(Optional.ofNullable(captured).orElse(""));
                exist[idx] = new Data.ToPhi(captured != null);
            }
        } else {
            groups = new Phi[]{new Data.ToPhi(matcher.group())};
            exist = new Phi[]{new Data.ToPhi(true)};
        }
        result.put("groups", new Data.ToPhi(groups));
        result.put("existing", new Data.ToPhi(exist));
    }

    private void blank(final Phi result) {
        result.put(
            EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION,
            this.take(EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION)
        );
        result.put(
            EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index.START,
            new Data.ToPhi(-1)
        );
        result.put(
            "from",
            new PhTerminator(
                new Data.ToPhi("Matched block does not exist, can't get 'from' position")
            )
        );
        result.put(
            "to",
            new PhTerminator(
                new Data.ToPhi("Matched block does not exist, can't get 'to' position")
            )
        );
        result.put(
            "groups",
            new PhTerminator(new Data.ToPhi("Matched block does not exist, can't get groups"))
        );
        result.put(
            "existing",
            new PhTerminator(new Data.ToPhi("Matched block does not exist, can't get groups"))
        );
    }
}
