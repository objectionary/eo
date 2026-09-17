/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * What every dispatch of a program turns out to be.
 *
 * <p>A dispatch takes a name from an object, so when that object's type is
 * known and it has such an attribute, the dispatch <em>is</em> that attribute:
 * a copy of it, which is what a pair in {@link Links} means. The body of
 * {@code and} is {@code .if} taken from {@code Φ.bool}, {@code if} is a member
 * of that package, so the body is a copy of {@code Φ.bool.if}. Where the
 * attribute is looked for is {@link Provided}'s business — the object itself,
 * its package, or behind its {@code φ}.</p>
 *
 * <p>A name read off the object the line is written in is the same question
 * with the receiver left out: {@code if > not} inside {@code [if] > bool}
 * takes {@code if} from the {@code bool} around it. There is nothing to look
 * the name up in, since the row already says what the name came out as, but
 * the arguments of that very application say what the void it came out as
 * holds — so the application stands in for its own receiver and {@link Filled}
 * is asked all the same.</p>
 *
 * <p>Nothing is guessed. A receiver whose type nothing describes is left
 * alone, and so is a receiver that is a void: {@code x.next} inside an object
 * that takes {@code x} is one object in the text and a different one for every
 * caller, and the pair that would settle it belongs to the call site rather
 * than here. A dispatch already spoken for is left alone too, since a pass is
 * only ever asked for what the last one could not answer.</p>
 *
 * @since 0.68.0
 * @todo #8777:90min Carry the arms through a body a walk goes behind.
 *  A read takes its arms from the row of the call it is written on, which
 *  leaves out the read whose call settled on an object whose own body is the
 *  choice. Of the 622 rows eo-runtime still keeps rooted at {@code Φ.bool.if}
 *  without arms, 488 read such a receiver and 133 read one rooted at a void
 *  that names no arms of its own. {@link Provided} walks behind the body,
 *  arrives at a name rooted at the void and says nothing of what it passed on
 *  the way, so the arms sitting on the body are lost before the read is asked.
 *  Handing them up wants the walk to give back where it went as well as where
 *  it ended.
 */
final class Dispatched {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Every dispatch of the program.
     */
    private final Collection<Site> all;

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> args;

    /**
     * The arguments of every application bound by name, from {@link Given}.
     */
    private final Map<String, Map<String, String>> named;

    /**
     * What every dispatch takes its attribute from, from {@link Taken}.
     */
    private final Map<String, String> receivers;

    /**
     * The locator of every void this pass may look into.
     */
    private final Collection<String> hollows;

    /**
     * Ctor.
     *
     * @param provides The provides table
     * @param dispatches Every dispatch of the program
     * @param arguments The arguments of every application, from {@link Given}
     * @param bindings The arguments of every application bound by name
     * @param taken What every dispatch takes its attribute from
     * @param voids The locator of every void this pass may look into, empty
     *  when it may look into none
     */
    Dispatched(
        final XML provides,
        final Collection<Site> dispatches,
        final Map<String, List<String>> arguments,
        final Map<String, Map<String, String>> bindings,
        final Map<String, String> taken,
        final Collection<String> voids
    ) {
        this.given = provides;
        this.all = dispatches;
        this.args = arguments;
        this.named = bindings;
        this.receivers = taken;
        this.hollows = voids;
    }

    /**
     * The pairs that follow from what is known, beyond what is known already.
     *
     * @param pairs The pairs, each name against the one it is a copy of
     * @return The dispatches answered this time, each against the attribute it
     *  turns out to be, empty when nothing further can be answered
     */
    Map<String, String> answers(final Map<String, String> pairs) {
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(this.given, names, this.hollows);
        final Filled filled = this.filled(pairs, names, owned);
        final Map<String, String> found = new HashMap<>(0);
        for (final Site dispatch : this.all) {
            final String made = dispatch.made();
            final String known = pairs.getOrDefault(made, "");
            if (known.isEmpty() || this.rooted(known)) {
                final String bearer = dispatch.bearer();
                final String kept;
                if (bearer.isEmpty()) {
                    kept = filled.instead(known, made, made);
                } else {
                    kept = filled.instead(
                        owned.attribute(names.getOrDefault(bearer, bearer), dispatch.name()),
                        bearer,
                        made
                    );
                }
                if (this.better(kept, known, made)) {
                    found.put(made, kept);
                }
            }
        }
        return found;
    }

    /**
     * The dispatches nothing else can answer, said as the tables say them.
     *
     * <p>A rewrite into what fills a void dies where the filling is not
     * settled yet, and {@link Filled} answers nothing rather than hand back
     * the name it was asked about (#8351). Where the passes have stopped
     * learning, no filling is going to settle either, and the name the tables
     * give is all there is to say: the {@code leaf} of whatever fills
     * {@code x}, rooted at the void and true of every caller. Asked last so
     * that a site is given up on only once, and only about a site no pair
     * covers, since a name already worked out is not worth replacing with the
     * one it was worked out from.</p>
     *
     * @param pairs The pairs, each name against the one it is a copy of
     * @return The dispatches nothing rewrote, each against the name the tables
     *  give it, empty when every one of them is answered already
     */
    Map<String, String> guesses(final Map<String, String> pairs) {
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(this.given, names, this.hollows);
        final Map<String, String> found = new HashMap<>(0);
        for (final Site dispatch : this.all) {
            final String made = dispatch.made();
            final String bearer = dispatch.bearer();
            if (!bearer.isEmpty() && !pairs.containsKey(made)) {
                final String kept = owned.attribute(
                    names.getOrDefault(bearer, bearer), dispatch.name()
                );
                if (this.better(kept, "", made)) {
                    found.put(made, kept);
                }
            }
        }
        return found;
    }

    /**
     * The dispatches that come back with one of several objects.
     *
     * <p>A dispatch on a void that holds a picker is answered by whatever the
     * call put there, and where the arms agree on nothing {@link Filled} has
     * an answer all the same: one of them, and no third thing. There is
     * nowhere to keep that while an answer is a locator, so it is asked for
     * here rather than inside {@link #answers(Map)}, once, by whoever writes
     * the rows and can hold two of them (#8744).</p>
     *
     * <p>Only a site left rooted at a void is asked. A site that settled on an
     * object settled on it because the arms agreed or because no void stood in
     * the way, and a choice is the poorer of the two answers wherever both are
     * to be had.</p>
     *
     * <p>A name taken off such a site is a choice of its own, so the arms are
     * handed on to the reads written on them for as long as anything is still
     * being learned: the {@code plus} of a call that came back with a
     * {@code dial} or a {@code clock} is one of two {@code plus}es. That is
     * asked of the row rather than of the void again, because the walk from
     * the read arrives at the call and stops there, while the call that filled
     * the void is wherever its own caller wrote it. All of the arms or none of
     * them, the way {@link Arrived} has it: an arm without the attribute leaves
     * the read rooted at the void it had, which is true of every caller and
     * says little, rather than with a choice that holds for some callers and
     * lies about the rest.</p>
     *
     * @param pairs The pairs, each name against the one it is a copy of
     * @return The arms, by the locator of the dispatch, without the dispatches
     *  that come back with one object or none
     */
    Map<String, Collection<String>> choices(final Map<String, String> pairs) {
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(this.given, names, this.hollows);
        final Filled filled = this.filled(pairs, names, owned);
        final Map<String, Collection<String>> found = new HashMap<>(0);
        for (final Site dispatch : this.all) {
            final String made = dispatch.made();
            final String bearer = dispatch.bearer();
            if (!bearer.isEmpty() && this.rooted(pairs.getOrDefault(made, ""))) {
                final Collection<String> arms = filled.choice(
                    owned.attribute(names.getOrDefault(bearer, bearer), dispatch.name()),
                    bearer,
                    made
                );
                if (!arms.isEmpty()) {
                    found.put(made, arms);
                }
            }
        }
        boolean more = true;
        while (more) {
            more = this.spread(found, pairs, owned);
        }
        return found;
    }

    private boolean spread(
        final Map<String, Collection<String>> found,
        final Map<String, String> pairs,
        final Provided owned
    ) {
        boolean more = false;
        for (final Site dispatch : this.all) {
            final String made = dispatch.made();
            final String bearer = dispatch.bearer();
            if (!found.containsKey(made) && found.containsKey(bearer)
                && this.rooted(pairs.getOrDefault(made, ""))) {
                final Collection<String> arms = new Arrived(owned).names(
                    found.get(bearer), dispatch.name()
                );
                if (arms.size() > 1) {
                    found.put(made, arms);
                    more = true;
                }
            }
        }
        return more;
    }

    private Filled filled(
        final Map<String, String> pairs, final Map<String, String> names,
        final Provided owned
    ) {
        final Map<String, Map<String, String>> bound = new Copied(
            new Bound(this.args, this.named, this.receivers, pairs, owned).all(),
            pairs,
            new Lent(owned, this.all, this.args, this.receivers).sites(names)
        ).all();
        return new Filled(
            pairs,
            owned,
            new Puts(bound, new Holders(bound, pairs).all()),
            this.hollows
        );
    }

    private boolean rooted(final String type) {
        return !this.hollows.isEmpty() && new Rooted(this.hollows).covers(type);
    }

    private boolean better(final String kept, final String known, final String made) {
        final boolean found;
        if (kept.isEmpty() || kept.equals(made) || kept.equals(known)) {
            found = false;
        } else {
            found = known.isEmpty() || !this.rooted(kept)
                || known.startsWith(kept.concat("."));
        }
        return found;
    }
}
