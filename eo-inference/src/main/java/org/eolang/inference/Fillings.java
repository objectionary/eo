/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * What the program puts into every void, read back from the links.
 *
 * <p>Every application says what it fills, and says it in its own row, where
 * {@link Bound} wrote it: a {@code bind} names the void and what went into it.
 * So the fact is in the tables already and lies the wrong way round — to learn
 * what one void is ever given, a reader walks the row of every object that
 * ever filled anything, and eo-runtime has 23,871 of those bindings.</p>
 *
 * <p>What goes in is gathered as a type rather than as a locator, which is
 * what makes the answer worth reading: the {@code φ} of {@code Φ.bytes} is
 * filled 7,752 times and all but a handful of those fillings are literals. As
 * types there are two of them, a datum and a {@code Φ.bytes}; as locators
 * there are 7,752. The handful arrive as a {@code Φ.bytes.as-bytes}, which is
 * a {@code Φ.bytes} and is counted as one by {@link Counted}, once the walk is
 * over.</p>
 *
 * <p>Which type a filling is counted as is {@link Landed}'s question, and it
 * has to be asked of the links rather than of {@link Ends} alone. An argument
 * is written afresh at every call site, so the same expression passed at
 * eleven places is eleven locators no chain of copies joins, and counting them
 * apart makes a void look filled eleven ways when it is filled one way eleven
 * times. Settling each of them first leaves {@code Φ.number.as-bytes} with
 * five members where it had more than a cap's worth, and the five are worth
 * reading.</p>
 *
 * <p>A filling whose walk runs into a void has no type of its own to give, and
 * it is not thereby nothing: what fills that void reaches this one too, a hop
 * further along. {@link Carried} walks the hop, so a void filled with an
 * {@code oak} by one caller and handed on from a void filled with a number is
 * seen to hold both, and a void whose every caller passes on a void of its own
 * is described by that rather than by silence.</p>
 *
 * <p>An atom fills a void too, and says so in a brace list {@link Handed}
 * reads. That is spent here and not later, because the two rules feed each
 * other: a hop says which formation an atom is handed, the atom says what
 * lands in that formation, and a hop carries it on from there. So the walk is
 * made again for as long as the atoms have anything left to add, and what they
 * add is put among the fillings a call site names rather than among the
 * answers, so that a void left holding the far end of a hop gives that up as
 * soon as a forma arrives for it (#8396).</p>
 *
 * <p>A call nobody can place fills a void too, and binds nothing to say so.
 * {@link Unplaced} says which names such calls hand arguments to, and every
 * void of a formation by one of those names holds, besides what the calls in
 * sight put there, an {@link Unknown}. That member is what keeps a void one
 * caller fills with an {@code oak} from being named an {@code oak} while a
 * call out of sight fills it with something else (#9006). A {@code ρ} is
 * left alone, since whoever dispatches fills it, placed or not.</p>
 *
 * @since 0.69.0
 */
final class Fillings {

    /**
     * What the links table says.
     */
    private final Said table;

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * The locator of every void.
     */
    private final Collection<String> hollows;

    /**
     * The calls nobody can place.
     */
    private final Unplaced unseen;

    /**
     * Ctor.
     *
     * @param links The links table, as {@link Resolved} left it
     * @param provides The provides table, which says where a filling can land
     */
    Fillings(final XML links, final XML provides) {
        this(
            new Said(new Pairs(links)), provides, new Hollows(provides).all(),
            new Unplaced(
                Collections.emptyList(), Collections.emptyMap(), Collections.emptyMap(),
                Collections.emptyList()
            )
        );
    }

    /**
     * Ctor.
     *
     * @param links What the links table says, as {@link Resolved} left it
     * @param provides The provides table, which says where a filling can land
     * @param voids The locator of every void, from {@link Hollows}
     * @param unplaced The calls nobody can place
     */
    Fillings(
        final Said links, final XML provides, final Collection<String> voids,
        final Unplaced unplaced
    ) {
        this.table = links;
        this.given = provides;
        this.hollows = voids;
        this.unseen = unplaced;
    }

    /**
     * What is ever put into every void.
     *
     * @return The types put in, by the locator of the void, without the voids
     *  nobody ever fills
     */
    Map<String, Collection<Type>> all() {
        final Map<String, Map<String, Type>> walked = this.walked();
        final Map<String, String> behaves = new Behaviours(this.given).all();
        final Map<String, Collection<Type>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Map<String, Type>> hollow : walked.entrySet()) {
            found.put(
                hollow.getKey(), new ArrayList<>(new Counted(hollow.getValue(), behaves).all())
            );
        }
        for (final String hollow : this.open(walked.keySet())) {
            found.computeIfAbsent(hollow, key -> new ArrayList<>(1)).add(new Unknown());
        }
        return found;
    }

    /**
     * What is ever put into every void that only calls in sight fill.
     *
     * <p>This is what a void may be named from. A void a call out of sight
     * fills is left out whatever else fills it, since the {@link Unknown}
     * {@link #all()} gives it is refused by {@link Sole} but read past by a
     * rule that looks only at the members that name something.</p>
     *
     * @return The types put in, by the locator of the void, without the voids
     *  nobody ever fills and the voids a call nobody can place fills
     */
    Map<String, Collection<Type>> closed() {
        final Map<String, Map<String, Type>> walked = this.walked();
        final Map<String, String> behaves = new Behaviours(this.given).all();
        final Collection<String> open = this.open(walked.keySet());
        final Map<String, Collection<Type>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Map<String, Type>> hollow : walked.entrySet()) {
            if (!open.contains(hollow.getKey())) {
                found.put(hollow.getKey(), new Counted(hollow.getValue(), behaves).all());
            }
        }
        return found;
    }

    /**
     * What every void is ever given, by the name each thing goes by.
     *
     * <p>This is the census before it is counted, and it is what a pass asks
     * when it wants to know whether a void holds a formation, or holds
     * anything at all. Asking a list of its own instead, built from the calls
     * alone, gave a void two atoms fill no filling while the census gave it
     * two, and an arm that read the void was struck as dead (#9006).</p>
     *
     * @return The names of what is put in, by the locator of the void, without
     *  the voids nobody ever fills
     */
    Map<String, Collection<String>> holders() {
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        final Map<String, Map<String, Type>> walked = this.walked();
        for (final Map.Entry<String, Map<String, Type>> hollow : walked.entrySet()) {
            found.put(hollow.getKey(), hollow.getValue().keySet());
        }
        for (final String hollow : this.open(walked.keySet())) {
            found.putIfAbsent(hollow, Collections.emptySet());
        }
        return found;
    }

    private Collection<String> open(final Collection<String> filled) {
        final Collection<String> names = this.unseen.names(
            new Ends(this.table.all()).names(), filled
        );
        final Collection<String> found = new LinkedHashSet<>(0);
        for (final String hollow : this.hollows) {
            if (!hollow.endsWith(".ρ") && names.contains(
                hollow.substring(0, Math.max(0, hollow.lastIndexOf('.')))
                    .replaceFirst("^.*\\.", "")
            )) {
                found.add(hollow);
            }
        }
        return found;
    }

    private Map<String, Map<String, Type>> walked() {
        final Map<String, String> names = new Ends(this.table.all()).names();
        final Map<String, String> landings = new Landed(this.table, this.given).all();
        final Forms forms = new Forms(this.table.forms());
        final Map<String, Map<String, Type>> placed = new LinkedHashMap<>(0);
        final Map<String, Map<String, Type>> handed = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Collection<String>> bound : this.table.puts().entrySet()) {
            for (final String put : bound.getValue()) {
                final String end = landings.get(put);
                if (end == null) {
                    final String stopped = names.getOrDefault(put, put);
                    handed.computeIfAbsent(bound.getKey(), key -> new LinkedHashMap<>(0))
                        .putIfAbsent(forms.name(stopped), forms.type(stopped));
                } else {
                    placed.computeIfAbsent(bound.getKey(), key -> new LinkedHashMap<>(0))
                        .putIfAbsent(forms.name(end), forms.type(end));
                }
            }
        }
        final Handed atoms = new Handed(
            this.given, new Provided(this.given, names, this.hollows)
        );
        Map<String, Map<String, Type>> walked = new Carried(placed, handed).all();
        while (atoms.fills(placed, walked)) {
            walked = new Carried(placed, handed).all();
        }
        return walked;
    }
}
