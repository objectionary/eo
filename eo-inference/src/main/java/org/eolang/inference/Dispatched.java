/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
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
 */
final class Dispatched {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * What the links table says, as the rules left it.
     */
    private final Said written;

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
     * Every object of the program that terminates, from {@link Dead}.
     */
    private final Collection<String> dead;

    /**
     * Ctor.
     *
     * @param provides The provides table
     * @param said What the links table says, as the rules left it, which the
     *  fillings of every pass are read against
     * @param dispatches Every dispatch of the program
     * @param arguments The arguments of every application, from {@link Given}
     * @param bindings The arguments of every application bound by name
     * @param taken What every dispatch takes its attribute from
     * @param voids The locator of every void this pass may look into, empty
     *  when it may look into none
     * @param ends Every object of the program that terminates, from
     *  {@link Dead}
     */
    Dispatched(
        final XML provides,
        final Said said,
        final Collection<Site> dispatches,
        final Map<String, List<String>> arguments,
        final Map<String, Map<String, String>> bindings,
        final Map<String, String> taken,
        final Collection<String> voids,
        final Collection<String> ends
    ) {
        this.given = provides;
        this.written = said;
        this.all = dispatches;
        this.args = arguments;
        this.named = bindings;
        this.receivers = taken;
        this.hollows = voids;
        this.dead = ends;
    }

    /**
     * The pairs that follow from what is known, beyond what is known already.
     *
     * @param pairs The pairs, each name against the one it is a copy of
     * @param copied The arms every read off a choice is a copy of, from
     *  {@link #copies(Map, Map)}
     * @return The dispatches answered this time, each against the attribute it
     *  turns out to be, empty when nothing further can be answered
     */
    Map<String, String> answers(
        final Map<String, String> pairs, final Map<String, Collection<String>> copied
    ) {
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(this.given, names, this.hollows);
        final Filled filled = this.filled(pairs, owned, copied);
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
                if (new Improved(this.hollows, known, made).on(kept)) {
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
                if (new Improved(this.hollows, "", made).on(kept)) {
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
     * here rather than inside {@link #answers(Map, Map)}, once, by whoever writes
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
     * lies about the rest. Which row carries them is {@link Borne}'s business,
     * since a receiver that settled on an object of its own keeps the choice
     * one step in, behind that object's body.</p>
     *
     * @param pairs The pairs, each name against the one it is a copy of
     * @param copied The arms every read off a choice is a copy of, from
     *  {@link #copies(Map, Map)}
     * @return The arms, by the locator of the dispatch, without the dispatches
     *  that come back with one object or none
     */
    Map<String, Collection<String>> choices(
        final Map<String, String> pairs, final Map<String, Collection<String>> copied
    ) {
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(this.given, names, this.hollows);
        final Filled filled = this.filled(pairs, owned, copied);
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
            more = this.spread(found, pairs, names, owned);
        }
        return found;
    }

    /**
     * The arms every read off a choice is a copy of, as far as they reach.
     *
     * <p>A call on a read off a choice fills the voids of every arm the read
     * is a copy of (#8883), and what fills a void is what a pass answers the
     * dispatches rooted at it from. The arms are a choice, though, and a
     * choice is worked out from those very fillings, so every arm found fills
     * a void that may make a choice of some further read. Asking for them on
     * every pass costs one more {@link Bound}, which is most of what a pass
     * costs, so they are asked for only where a pass would otherwise be the
     * last, and asked again until no arm is added (#8993).</p>
     *
     * @param pairs The pairs, each name against the one it is a copy of
     * @param known The arms found already
     * @return The arms, by the locator of the read, the known ones among them
     */
    Map<String, Collection<String>> copies(
        final Map<String, String> pairs, final Map<String, Collection<String>> known
    ) {
        final Map<String, Collection<String>> found = new HashMap<>(known);
        boolean more = !this.hollows.isEmpty();
        while (more) {
            more = false;
            for (final Map.Entry<String, Collection<String>> read
                : new Copied(this.all, this.choices(pairs, found)).all().entrySet()) {
                final Collection<String> arms = new LinkedHashSet<>(
                    found.getOrDefault(read.getKey(), Collections.emptyList())
                );
                if (arms.addAll(read.getValue())) {
                    found.put(read.getKey(), arms);
                    more = true;
                }
            }
        }
        return found;
    }

    private boolean spread(
        final Map<String, Collection<String>> found,
        final Map<String, String> pairs,
        final Map<String, String> names,
        final Provided owned
    ) {
        boolean more = false;
        for (final Site dispatch : this.all) {
            final String made = dispatch.made();
            if (!found.containsKey(made) && !dispatch.bearer().isEmpty()
                && this.rooted(pairs.getOrDefault(made, ""))) {
                final Collection<String> arms = new Arrived(owned).names(
                    new Borne(found, names, owned).arms(dispatch), dispatch.name()
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
        final Map<String, String> pairs, final Provided owned,
        final Map<String, Collection<String>> copied
    ) {
        final Map<String, Map<String, String>> bound = new Bound(
            this.args, this.named, this.receivers, this.all, pairs, owned, copied
        ).all();
        return new Filled(
            pairs,
            owned,
            new Puts(
                bound,
                new Fillings(this.written.with(pairs, bound), this.given, this.hollows).holders(),
                this.dead
            ),
            this.hollows
        );
    }

    private boolean rooted(final String type) {
        return !this.hollows.isEmpty() && new Rooted(this.hollows).covers(type);
    }
}
