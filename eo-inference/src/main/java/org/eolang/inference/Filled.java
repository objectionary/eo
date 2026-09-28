/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * What a name taken from a void turns out to be, where the void is filled.
 *
 * <p>{@link Provided} answers a question about a void with the void itself:
 * the {@code next} of whatever fills {@code x} is {@code Φ.inc.x.next}, which
 * is true of every caller and concrete for none. A receiver reached through an
 * application is a caller, though, and the argument it put in that place says
 * what the void holds. So the void is taken out of the answer, what fills it is
 * put in, and the rest of the names are asked again one by one — the {@code
 * next} of a {@code t} rather than the {@code next} of whatever fills
 * {@code x}.</p>
 *
 * <p>The fillings are gathered along the whole chain a receiver resolves
 * through, since the application that fills a void and the dispatch that asks
 * about it are rarely the same object: {@code inc t > held} fills the void and
 * {@code held.next} asks. A void filled nearer the question wins over one
 * filled further away, and a void nobody fills keeps the answer as it was.</p>
 *
 * <p>The chain is followed through the pairs as well as through the bodies,
 * since what a receiver turns out to be is often worked out a pass later than
 * the application that filled the void: a name settled this pass leads to an
 * object whose voids were filled the pass before, and the fillings are wanted
 * from both ends of that.</p>
 *
 * <p>An answer rooted at a void nothing here fills is not the end of it
 * either. The void may hold a formation that hands back what it is given, in
 * which case the answer is one of the things a call put in, and
 * {@link Branched} says which. Which call is the question, since the chain a
 * receiver resolves through reaches other applications of the same object and
 * their arguments went into their own copies of the void. The call that landed
 * here is asked first — the one that filled a void of a formation this void
 * holds — so that a reader further along the chain learns what that call was
 * handed (#8508). Where its arms agree on nothing the next call up the chain
 * is asked, and then the next, and when no call of its own agrees the fillings
 * gathered along the whole chain are joined instead.</p>
 *
 * <p>Those later calls are strangers, and an answer taken from one of them is
 * a guess: their arguments went into copies of the void this question is not
 * about. The guess holds because an argument relayed into a copy of the same
 * void is written for the same shape, and it is worth keeping because giving
 * it up leaves hundreds of names rooted at a void again while settling almost
 * nothing (#8571).</p>
 *
 * <p>No stranger is asked about a call that takes the void itself, though,
 * such as the {@code if} of a boolean. Its arguments are the only ones that
 * went into the formations the void holds on its behalf, and the calls up the
 * chain of its receiver are the ones that made the receiver: the {@code or}
 * that a {@code tuple.at} chooses with makes its own choice of two booleans,
 * and asking it made every {@code at} a boolean. The guess is kept for a name
 * read off what the void holds, which is where the call that filled it is
 * further up the chain (#8552).</p>
 *
 * <p>A walk that dies answers nothing at all, rather than handing back the
 * name it was asked about. The two are not the same question: a void nobody
 * fills is the answer, while a void this call fills with something the passes
 * have not settled yet is an answer nobody has worked out. Writing the second
 * one down as if it were the first froze it, since {@link Dispatched} asks
 * again only about a name rooted at a void and takes one rooted answer for
 * another only when the second stands under the first. The {@code if} of a
 * {@code recovered} is a {@code Φ.bool.if}, which is rooted at a void as well,
 * so the site kept the name of a void the line above it fills (#8351).</p>
 *
 * <p>Only the arms of those formations are counted. An argument is relayed to
 * every formation the void might hold, because which one it turns out to be is
 * not known where the argument is written, and a single stray relay among the
 * arms is enough to make {@link Branched} give up on all of them: the
 * {@code if} of an {@code abs} puts its {@code 0.plus value} into the two
 * choices of a boolean and into the {@code b} of {@code Φ.bytes.eq} as
 * well.</p>
 *
 * @since 0.69.0
 */
final class Filled {

    /**
     * What the calls of the program put into its voids.
     */
    private final Puts puts;

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> pairs;

    /**
     * The provides table, by the name a type goes by.
     */
    private final Provided owned;

    /**
     * The locator of every void.
     */
    private final Collection<String> hollows;

    /**
     * Ctor.
     *
     * @param links The pairs, each name against the one it is a copy of
     * @param provided The provides table, by the name a type goes by
     * @param bound What the calls of the program put into its voids
     * @param voids The locator of every void, from {@link Hollows}
     */
    Filled(
        final Map<String, String> links,
        final Provided provided,
        final Puts bound,
        final Collection<String> voids
    ) {
        this.puts = bound;
        this.pairs = links;
        this.owned = provided;
        this.hollows = voids;
    }

    /**
     * What this answer turns out to be for this receiver.
     *
     * @param answer The type of the attribute, as the table gave it
     * @param bearer The locator of the receiver the question was asked of
     * @param site The locator of the call the question is asked at
     * @return The type the answer stands for here, the answer itself when no
     *  caller says what the void holds, or an empty string when a caller says
     *  and the walk into what it put there has nowhere to go yet
     */
    String instead(final String answer, final String bearer, final String site) {
        return this.instead(answer, bearer, site, new HashSet<>(0));
    }

    /**
     * The objects this answer may be, where it may be more than one.
     *
     * <p>A call on a void that holds a picker comes back with what the call
     * put there, and where the arms agree on nothing there is still this to
     * say: it is one of them and no third thing. Asked apart from
     * {@link #instead(String, String, String)} because the two want different
     * halves of the same walk — one the agreement, the other the arms it was
     * looked for in — and because only a row that can hold two answers has
     * anywhere to put this (#8744).</p>
     *
     * <p>Whatever the answer asked of the void asks beyond it is asked of every
     * arm in turn, so that the {@code listen} of a call that hands back a dial
     * or a clock is a choice between two {@code listen}s rather than a choice
     * between two objects a reader has to finish the question on. An arm with
     * no such attribute ends it: a choice is worth having only while every
     * member of it is an answer.</p>
     *
     * @param answer The type of the attribute, as the table gave it
     * @param bearer The locator of the receiver the question was asked of
     * @param site The locator of the call the question is asked at
     * @return The locators, empty when the answer is one object or none
     */
    Collection<String> choice(final String answer, final String bearer, final String site) {
        final String root = new Rooted(this.hollows).names(answer);
        Collection<String> found = Collections.emptyList();
        if (!root.isEmpty()) {
            found = new Arrived(this.owned).names(
                this.chosen(root, answer, bearer, site),
                answer.substring(Math.min(root.length() + 1, answer.length()))
            );
        }
        if (found.size() < 2) {
            found = Collections.emptyList();
        }
        return found;
    }

    private String instead(
        final String answer, final String bearer, final String site,
        final Collection<String> seen
    ) {
        final Map<String, String> fillings = this.fillings(bearer);
        final String found;
        if (fillings.containsKey(answer)) {
            found = fillings.get(answer);
        } else {
            String longest = "";
            for (final String hollow : fillings.keySet()) {
                if (answer.startsWith(hollow.concat("."))
                    && hollow.length() > longest.length()) {
                    longest = hollow;
                }
            }
            if (longest.isEmpty()) {
                found = this.branch(answer, fillings, bearer, site, seen);
            } else {
                found = new Arrived(this.owned).names(
                    fillings.get(longest), answer.substring(longest.length() + 1)
                );
            }
        }
        return found;
    }

    private String branch(
        final String answer, final Map<String, String> fillings, final String bearer,
        final String site, final Collection<String> seen
    ) {
        final String root = new Rooted(this.hollows).names(answer);
        String found = answer;
        if (!root.isEmpty()) {
            final String handed = this.handed(root, answer, fillings, bearer, site);
            if (!handed.isEmpty() && seen.add(handed)) {
                found = this.through(answer, root, handed, site, seen);
            }
        }
        return found;
    }

    private String handed(
        final String root, final String answer, final Map<String, String> fillings,
        final String bearer, final String site
    ) {
        String found = "";
        for (final String call : this.calls(root, answer, bearer, site)) {
            final Map<String, String> arms = this.puts.armed(this.arms(call), root);
            if (!arms.isEmpty()) {
                found = new Branched(this.owned, arms, this.hollows, this.puts).names();
                if (!found.isEmpty()) {
                    break;
                }
            }
        }
        if (found.isEmpty() && !root.equals(answer)) {
            found = new Branched(
                this.owned, this.puts.armed(fillings, root), this.hollows, this.puts
            ).names();
        }
        return found;
    }

    private Collection<String> chosen(
        final String root, final String answer, final String bearer, final String site
    ) {
        Collection<String> found = Collections.emptyList();
        for (final String call : this.calls(root, answer, bearer, site)) {
            final Map<String, String> arms = this.puts.armed(this.arms(call), root);
            if (!arms.isEmpty()) {
                final Collection<String> given =
                    new Branched(this.owned, arms, this.hollows, this.puts).whole();
                if (given.size() > 1) {
                    found = given;
                    break;
                }
            }
        }
        if (found.isEmpty() && !root.equals(answer)) {
            found = new Branched(
                this.owned, this.puts.armed(this.fillings(bearer), root), this.hollows,
                this.puts
            ).whole();
        }
        return found;
    }

    private Collection<String> calls(
        final String root, final String answer, final String bearer, final String site
    ) {
        final Collection<String> found = new LinkedHashSet<>(0);
        found.add(site);
        final Collection<String> seen = new HashSet<>(0);
        String walked = bearer;
        if (root.equals(answer)) {
            walked = "";
        }
        while (!walked.isEmpty() && seen.add(walked)) {
            found.add(walked);
            if (this.pairs.containsKey(walked)) {
                walked = this.pairs.get(walked);
            } else {
                walked = this.owned.body(walked);
            }
        }
        return found;
    }

    private String through(
        final String answer, final String root, final String handed, final String site,
        final Collection<String> seen
    ) {
        String found = new Arrived(this.owned).names(
            handed, answer.substring(Math.min(root.length() + 1, answer.length()))
        );
        if (found.isEmpty()) {
            found = this.instead(answer, handed, site, seen);
        }
        return found;
    }

    private Map<String, String> arms(final String site) {
        final Map<String, String> found = new HashMap<>(0);
        for (final Map.Entry<String, String> fill
            : this.puts.at(site).entrySet()) {
            found.put(fill.getKey(), new Ends(this.pairs).name(fill.getValue()));
        }
        return found;
    }

    private Map<String, String> fillings(final String bearer) {
        final Map<String, String> found = new HashMap<>(0);
        final Collection<String> seen = new HashSet<>(0);
        String walked = bearer;
        while (seen.add(walked)) {
            this.gathered(found, walked);
            if (!this.pairs.containsKey(walked)) {
                break;
            }
            walked = this.pairs.get(walked);
        }
        final Map<String, String> through = new HashMap<>(found.size());
        for (final Map.Entry<String, String> fill : found.entrySet()) {
            final Collection<String> passed = new HashSet<>(0);
            String reached = fill.getValue();
            while (found.containsKey(reached) && passed.add(reached)) {
                reached = found.get(reached);
            }
            through.put(fill.getKey(), reached);
        }
        return through;
    }

    private void gathered(final Map<String, String> found, final String type) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = type;
        while (!walked.isEmpty() && seen.add(walked)) {
            for (final Map.Entry<String, String> fill
                : this.puts.at(walked).entrySet()) {
                found.putIfAbsent(fill.getKey(), new Ends(this.pairs).name(fill.getValue()));
            }
            if (this.pairs.containsKey(walked)) {
                walked = this.pairs.get(walked);
            } else {
                walked = this.owned.body(walked);
            }
        }
    }
}
