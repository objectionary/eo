/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Attrs;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.PhTerminator;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Regex.pattern.match.matched-from-index.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "regex.pattern.match.matched-from-index")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOregex$EOpattern$EOmatch$EOmatched_from_index extends PhDefault
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
    public EOregex$EOpattern$EOmatch$EOmatched_from_index() {
        super(
            new Attrs(
                new Attr(
                    EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION,
                    new AtVoid(EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION)
                ),
                new Attr(
                    EOregex$EOpattern$EOmatch$EOmatched_from_index.START,
                    new AtVoid(EOregex$EOpattern$EOmatch$EOmatched_from_index.START)
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
        } catch (final IOException | ClassNotFoundException ex) {
            throw new ExFailure("cannot deserialize the compiled regex pattern", ex);
        }
        final int start = new Expect.Natural(
            Expect.at(this, EOregex$EOpattern$EOmatch$EOmatched_from_index.START)
        ).it();
        if (start > text.length()) {
            throw new ExFailure(
                "the 'start' attribute (%d) must be less than or equal to text length (%d)",
                start,
                text.length()
            );
        }
        final boolean found = matcher.find(start);
        final Phi result = match.take("matched");
        if (found) {
            this.fill(result, matcher);
        } else {
            this.blank(result);
        }
        return result;
    }

    /**
     * Fill the matched block with the data of a real match.
     * @param result The matched block to fill
     * @param matcher The matcher positioned on the found subsequence
     */
    private void fill(final Phi result, final Matcher matcher) {
        result.put(
            EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION,
            this.take(EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION)
        );
        result.put(
            EOregex$EOpattern$EOmatch$EOmatched_from_index.START,
            this.take(EOregex$EOpattern$EOmatch$EOmatched_from_index.START)
        );
        result.put("from", new Data.ToPhi(matcher.start()));
        result.put("to", new Data.ToPhi(matcher.end()));
        final Phi[] groups;
        if (matcher.groupCount() > 0) {
            groups = new Phi[matcher.groupCount() + 1];
            for (int idx = 0; idx < groups.length; ++idx) {
                final String captured = matcher.group(idx);
                groups[idx] = new Data.ToPhi(captured == null ? "" : captured);
            }
        } else {
            groups = new Phi[]{new Data.ToPhi(matcher.group())};
        }
        result.put("groups", new Data.ToPhi(groups));
    }

    /**
     * Fill the matched block as a non-existent one: start is -1 and the from,
     * to and groups fields hold ⊥, so any attempt to read them terminates
     * the program with an explanatory cause.
     * @param result The matched block to fill
     */
    private void blank(final Phi result) {
        result.put(
            EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION,
            this.take(EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION)
        );
        result.put(
            EOregex$EOpattern$EOmatch$EOmatched_from_index.START,
            new Data.ToPhi(-1)
        );
        result.put(
            "from",
            PhTerminator.withCause("Matched block does not exist, can't get 'from' position")
        );
        result.put(
            "to",
            PhTerminator.withCause("Matched block does not exist, can't get 'to' position")
        );
        result.put(
            "groups",
            PhTerminator.withCause("Matched block does not exist, can't get groups")
        );
    }
}
