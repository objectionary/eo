/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
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
 * their arguments went into their own copies of the void. So the call asked is
 * the one that landed here — the one that filled a void of a formation this
 * void holds — and a reader further along the chain learns what that call was
 * handed (#8508).</p>
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
     * @return The type the answer stands for here, or the answer itself when
     *  no caller says what the void holds
     */
    String instead(final String answer, final String bearer, final String site) {
        return this.instead(answer, bearer, site, new HashSet<>(0));
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
                found = this.asked(
                    fillings.get(longest), answer.substring(longest.length() + 1), answer
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
            final String handed = this.handed(root, fillings, bearer, site);
            if (!handed.isEmpty() && seen.add(handed)) {
                found = this.through(answer, root, handed, site, seen);
            }
        }
        return found;
    }

    private String handed(
        final String root, final Map<String, String> fillings, final String bearer,
        final String site
    ) {
        String found = "";
        for (final String call : this.calls(bearer, site)) {
            final Map<String, String> arms = this.armed(this.arms(call), root);
            if (!arms.isEmpty()) {
                found = new Branched(this.owned, arms, this.hollows).names();
                if (!found.isEmpty()) {
                    break;
                }
            }
        }
        if (found.isEmpty()) {
            found = new Branched(
                this.owned, this.armed(fillings, root), this.hollows
            ).names();
        }
        return found;
    }

    private Map<String, String> armed(final Map<String, String> arms, final String root) {
        final Collection<String> holders = this.puts.holders(root);
        final Map<String, String> found = new HashMap<>(0);
        for (final Map.Entry<String, String> arm : arms.entrySet()) {
            final int dot = arm.getKey().lastIndexOf('.');
            if (dot > 0 && holders.contains(arm.getKey().substring(0, dot))) {
                found.put(arm.getKey(), arm.getValue());
            }
        }
        return found;
    }

    private Collection<String> calls(final String bearer, final String site) {
        final Collection<String> found = new LinkedHashSet<>(0);
        found.add(site);
        final Collection<String> seen = new HashSet<>(0);
        String walked = bearer;
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
        String found = this.asked(
            handed, answer.substring(Math.min(root.length() + 1, answer.length())), answer
        );
        if (found.equals(answer)) {
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

    private String asked(final String start, final String names, final String back) {
        String walked = start;
        int from = 0;
        while (from < names.length() && !walked.isEmpty()) {
            int next = names.indexOf('.', from);
            if (next < 0) {
                next = names.length();
            }
            walked = this.owned.attribute(walked, names.substring(from, next));
            from = next + 1;
        }
        if (walked.isEmpty()) {
            walked = back;
        }
        return walked;
    }
}
