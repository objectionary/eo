/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * What a void is filled with, where one of the fillings waits on the void.
 *
 * <p>{@code directory} keeps a {@code file} void and a {@code tmpfile} that
 * chooses on the {@code exists} of it, and that {@code tmpfile} is one of the
 * two things the program ever puts into that void. So what the void holds is
 * worked out from a filling whose own type is worked out from the void, and
 * the two wait on each other for as long as the passes run. 1,002 rows of
 * eo-runtime end at that void and go no further (#8565).</p>
 *
 * <p>The choice such a filling stands on is the loose end. One of its arms
 * terminates and hands nothing back, so a caller holding a value from the
 * choice got it from the arm that is left, which is what {@link Branched}
 * already says of a choice whose receiver is known. Reading that arm asks
 * nothing of the void — only the receiver of the choice does — so the filling
 * is settled from its arms, stands in the choice in place of itself, and the
 * void is asked what it holds with nothing left waiting on it. What the
 * filling itself is worth is settled a pass later, off a void that no longer
 * waits for it, and the choice then says the same thing again without any of
 * this.</p>
 *
 * <p>It is the body of a filling that is read this way and never a call made
 * on the void. The {@code fallback} of {@code Φ.tuple.at} is a terminating arm
 * at every call site of that {@code at}, and the arm left standing there is
 * the index rather than the element: an application whose arms are all but one
 * terminating is a choice where a choice is what stands on it, and nothing in
 * particular anywhere else.</p>
 *
 * @since 0.73.0
 */
final class Freed {

    /**
     * What the program was seen putting into every void, from
     * {@link Fillings}.
     */
    private final Map<String, Collection<Type>> told;

    /**
     * What the links table says.
     */
    private final Said table;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> arms;

    /**
     * Ctor.
     *
     * @param witnesses What the program was seen putting into every void,
     *  from {@link Fillings}
     * @param said What the links table says
     * @param provided What the types certainly have
     * @param arguments The arguments of every application, from {@link Given}
     */
    Freed(
        final Map<String, Collection<Type>> witnesses,
        final Said said,
        final Provided provided,
        final Map<String, List<String>> arguments
    ) {
        this.told = witnesses;
        this.table = said;
        this.owned = provided;
        this.arms = arguments;
    }

    /**
     * What is put into every void, with the fillings that wait on it settled.
     *
     * @return The types put in, by the locator of the void, in the order they
     *  were seen
     */
    Map<String, Collection<Type>> all() {
        final Map<String, Collection<Type>> found = new LinkedHashMap<>(this.told.size());
        for (final Map.Entry<String, Collection<Type>> hollow : this.told.entrySet()) {
            found.put(hollow.getKey(), this.members(hollow.getKey(), hollow.getValue()));
        }
        return found;
    }

    private Collection<Type> members(final String hollow, final Collection<Type> witnesses) {
        final Forms forms = new Forms(this.table.forms());
        final Ends ends = new Ends(this.table.all());
        final Collection<Type> found = new ArrayList<>(witnesses.size());
        for (final Type one : witnesses) {
            final String stood = this.stands(hollow, one.names());
            if (stood.isEmpty()) {
                found.add(one);
            } else {
                found.add(forms.type(ends.name(stood)));
            }
        }
        return found;
    }

    private String stands(final String hollow, final String member) {
        final String body = this.owned.body(member);
        final String answer = this.table.all().getOrDefault(body, "");
        final String found;
        if (!body.isEmpty()
            && (answer.equals(hollow) || answer.startsWith(hollow.concat(".")))) {
            found = this.sole(body);
        } else {
            found = "";
        }
        return found;
    }

    private String sole(final String call) {
        final Map<String, String> forms = this.table.forms();
        boolean dead = false;
        String found = "";
        for (final String arm : this.arms.getOrDefault(call, Collections.emptyList())) {
            if (arm.isEmpty()) {
                continue;
            }
            if ("terminator".equals(forms.get(arm))) {
                dead = true;
            } else if (found.isEmpty()) {
                found = arm;
            } else {
                found = "";
                break;
            }
        }
        if (!dead) {
            found = "";
        }
        return found;
    }
}
