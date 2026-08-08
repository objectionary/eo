/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Optional;
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
 * @todo #6388:90min Build an EO-native literal matcher as the first real step off
 *  {@link java.util.regex.Matcher}. {@code compile} already parses the envelope in EO
 *  and exposes the {@code body}/{@code flags} pair as its own attributes, so
 *  start a new {@code string/regex/} EO object that walks {@code txt} for an exact
 *  literal substring of {@code body} (no metacharacters yet) and fills the same
 *  {@code matched} protocol this atom fills today (position, start, from, to, groups,
 *  next, text, count, exists). Wire it as the real implementation for patterns that
 *  contain none of {@code . ^ $ * + ? ( ) [ ] \ |}, falling back to this atom otherwise,
 *  so the swap is incremental and every existing test keeps passing throughout.
 * @todo #6388:90min Add {@code ^}/{@code $} anchors and the {@code .} wildcard to the
 *  EO matcher from the puzzle above.
 * @todo #6388:90min Add character classes ({@code [abc]}, {@code [^abc]}, and the
 *  predefined {@code \d \w \s} and their negations) to the EO matcher.
 * @todo #6388:90min Add quantifiers ({@code * + ? {n,m}}, greedy only — Java's
 *  reluctant/possessive forms are out of scope until a real caller asks for them) to
 *  the EO matcher.
 * @todo #6388:90min Add capturing groups to the EO matcher, filling {@code groups} and
 *  {@code group(index)} the way {@link #fill(Phi, Matcher, String)} does today.
 * @todo #6388:90min Add alternation ({@code |}) to the EO matcher.
 * @todo #6388:90min Apply the six flag semantics ({@code /d /i /m /s /u /x} — see
 *  {@code regex.eo}'s header) to the EO matcher; today they only ever reach
 *  {@link java.util.regex.Pattern} as an inline {@code (?flags)} prefix.
 * @todo #6388:90min Handle supplementary characters (surrogate pairs) and the
 *  zero-width-match advancement {@code regex.eo}'s {@code next} already encodes, then
 *  delete this class, {@code EOregex$EOcompile$EOassembled}, and the {@code +rt jvm}
 *  and {@code +rt node} split in {@code regex.eo} — the tests already sitting in
 *  {@code regex.eo} are the acceptance criteria and must pass unchanged against
 *  the EO-only engine.
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
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
        } catch (final IOException | ClassNotFoundException | ClassCastException ex) {
            throw new ExFailure("cannot deserialize the compiled regex pattern", ex);
        }
        final int start = new Expect.Natural(
            Expect.at(this, EOregex$EOpattern$EOmatch$EOmatched_from_index.START)
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

    /**
     * Fill the matched block with the data of a real match.
     * @param result The matched block to fill
     * @param matcher The matcher positioned on the found subsequence
     * @param text Matched text
     */
    private void fill(final Phi result, final Matcher matcher, final String text) {
        result.put(
            EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION,
            this.take(EOregex$EOpattern$EOmatch$EOmatched_from_index.POSITION)
        );
        result.put(
            EOregex$EOpattern$EOmatch$EOmatched_from_index.START,
            this.take(EOregex$EOpattern$EOmatch$EOmatched_from_index.START)
        );
        result.put("from", new Data.ToPhi(text.codePointCount(0, matcher.start())));
        result.put("to", new Data.ToPhi(text.codePointCount(0, matcher.end())));
        final Phi[] groups;
        if (matcher.groupCount() > 0) {
            groups = new Phi[matcher.groupCount() + 1];
            for (int idx = 0; idx < groups.length; ++idx) {
                groups[idx] = new Data.ToPhi(
                    Optional.ofNullable(matcher.group(idx)).orElse("")
                );
            }
        } else {
            groups = new Phi[]{new Data.ToPhi(matcher.group())};
        }
        result.put("groups", new Data.ToPhi(groups));
    }

    /**
     * Fill the matched block as a non-existent one: start is -1 and the from,
     * to and groups fields hold bottom, so any attempt to read them terminates
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
