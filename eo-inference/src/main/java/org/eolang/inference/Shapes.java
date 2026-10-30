/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * The formations of a program, and the shapes the ones with no name show.
 *
 * <p>An answer may name a formation that has no name of its own: a
 * {@code φ}, an {@code αN}, or a name the parser made, {@code a🌵},
 * {@code p🌵} or {@code n🌵}. Its locator tells a reader nothing, so a page
 * shows its shape instead: its voids in brackets, <code>&#x2205;</code> for
 * each and {@code ^} for {@code ρ}, what its {@code @} answers after
 * <code>&#x2192;</code>, and a line for each attribute it binds under a name
 * the parser did not make. The line of an attribute that is a formation has
 * its voids, its name and what its {@code @} answers; any other attribute's
 * line has its name and what it answers. The formation being shown is
 * <code>&#x21BA;</code> wherever it is an answer, and any other answer that
 * is such a formation shows its voids and what its {@code @} answers, one
 * step and no further:</p>
 *
 * <pre> [&#x2205;] &#x2192; Φ.string
 *   [^ &#x2205;] &gt; read &#x21BA;</pre>
 *
 * <p>Formations of one shape are one answer, and one name when a place is
 * put in a {@link Band}; what their voids are called does not count, since a
 * call fills voids in order. A page caps what it shows: past three answers it
 * says how many there are, and past four attributes it says how many more
 * there are.</p>
 *
 * @since 0.76.0
 */
final class Shapes {

    /**
     * The last part of a locator that names nothing a reader wrote.
     */
    private static final Pattern HIDDEN = Pattern.compile("(φ|ρ|α\\d+|[apn]🌵.*)");

    /**
     * What the formation being shown is called inside its own shape.
     */
    private static final String ITSELF = "↺";

    /**
     * What goes between the lines of a shape: a new line and an indent.
     */
    private static final String BREAK = String.valueOf('\n').concat("  ");

    /**
     * The formations, by locator.
     */
    private final Map<String, Xnav> formations;

    /**
     * The file each formation is written in, by locator.
     */
    private final Map<String, String> files;

    /**
     * What the table says of each place.
     */
    private final Table table;

    /**
     * Ctor.
     *
     * @param found The formations, by locator: every object with a locator
     *  and no base, in every file
     * @param written The file each formation is written in, by locator
     * @param answers What the table says of each place
     */
    Shapes(final Map<String, Xnav> found, final Map<String, String> written, final Table answers) {
        this.formations = found;
        this.files = written;
        this.table = answers;
    }

    /**
     * Whether a label is a formation shown by its shape at a place.
     *
     * @param label The label
     * @param place The locator of the place, which shows its own locator as
     *  it is
     * @return TRUE when the label names a formation with no name of its own
     *  and is not the place itself
     */
    boolean hidden(final String label, final String place) {
        return !label.equals(place) && this.nameless(label);
    }

    /**
     * The shape of a formation.
     *
     * @param label The locator of the formation
     * @param cap Whether to cap the answers and the attributes, as a mark does
     * @return The shape, a line per attribute after the first
     */
    String shown(final String label, final boolean cap) {
        final List<String> lines = new ArrayList<>(1);
        lines.add(
            this.head(label).concat(Shapes.arrow(this.gives(label.concat(".φ"), label), cap))
        );
        final List<Xnav> bound = this.kids(label).stream()
            .filter(kid -> !"∅".equals(new Noted(kid).says("base")))
            .filter(kid -> !new Noted(kid).says("name").isEmpty())
            .filter(kid -> !Shapes.HIDDEN.matcher(new Noted(kid).says("name")).matches())
            .toList();
        final int shown;
        if (cap) {
            shown = Math.min(bound.size(), 4);
        } else {
            shown = bound.size();
        }
        for (final Xnav kid : bound.subList(0, shown)) {
            final String loc = new Noted(kid).says("loc");
            final String name = new Noted(kid).says("name");
            if (this.formations.containsKey(loc)) {
                lines.add(
                    String.format("%s > %s", this.head(loc), name).concat(
                        Shapes.arrow(this.gives(loc.concat(".φ"), label), cap)
                    )
                );
            } else {
                lines.add(name.concat(Shapes.arrow(this.gives(loc, label), cap)));
            }
        }
        if (cap && bound.size() > 4) {
            lines.add(String.format("and %d more", bound.size() - 4));
        }
        return String.join(Shapes.BREAK, lines);
    }

    /**
     * What a place answers, each formation with no name of its own folded
     * into its shape.
     *
     * @param place The locator of the place
     * @return The labels, formations of one shape being one label
     */
    Set<String> folded(final String place) {
        final Set<String> found = new LinkedHashSet<>(1);
        for (final String label : this.table.said(place).keySet()) {
            if (this.hidden(label, place)) {
                found.add(this.shown(label, false));
            } else {
                found.add(label);
            }
        }
        return found;
    }

    /**
     * The file a formation is written in.
     *
     * @param label The locator of the formation
     * @return The path of the source file, relative to the program
     */
    String file(final String label) {
        return this.files.get(label);
    }

    /**
     * The line a formation is written on.
     *
     * @param label The locator of the formation
     * @return The line, empty when the parser gave none
     */
    String line(final String label) {
        return new Noted(this.formations.get(label)).says("line");
    }

    private boolean nameless(final String label) {
        return this.formations.containsKey(label)
            && Shapes.HIDDEN.matcher(label.substring(label.lastIndexOf('.') + 1)).matches();
    }

    private List<Xnav> kids(final String label) {
        return this.formations.get(label).elements(Filter.withName("o")).toList();
    }

    private String head(final String label) {
        return this.kids(label).stream()
            .filter(kid -> "∅".equals(new Noted(kid).says("base")))
            .map(kid -> Shapes.mark(new Noted(kid).says("name")))
            .collect(Collectors.joining(" ", "[", "]"));
    }

    private static String mark(final String name) {
        final String found;
        if ("ρ".equals(name)) {
            found = "^";
        } else {
            found = "∅";
        }
        return found;
    }

    private List<String> gives(final String place, final String own) {
        return this.ordered(place).stream()
            .map(label -> this.step(label, own))
            .distinct()
            .toList();
    }

    private String step(final String label, final String own) {
        final String found;
        if (label.equals(own) || !this.nameless(label)) {
            found = this.called(label, own);
        } else {
            final List<String> then = this.ordered(label.concat(".φ")).stream()
                .map(what -> this.called(what, own))
                .distinct()
                .toList();
            if (then.isEmpty()) {
                found = this.head(label);
            } else {
                found = String.format("%s → %s", this.head(label), String.join(" or ", then));
            }
        }
        return found;
    }

    private String called(final String label, final String own) {
        final String found;
        if (label.equals(own)) {
            found = Shapes.ITSELF;
        } else {
            found = this.brief(label);
        }
        return found;
    }

    private String brief(final String label) {
        final Label said = new Label(label);
        final String found;
        if ("name".equals(said.kind()) && this.nameless(label)) {
            found = this.head(label);
        } else if ("fails".equals(said.kind())) {
            found = "a read that fails";
        } else {
            found = said.words();
        }
        return found;
    }

    private List<String> ordered(final String place) {
        final Map<String, Integer> said = this.table.said(place);
        return said.keySet().stream().sorted(
            Comparator.comparingInt((String label) -> -said.get(label))
                .thenComparing(new CodeOrder())
        ).toList();
    }

    private static String arrow(final List<String> said, final boolean cap) {
        final String found;
        if (said.isEmpty()) {
            found = "";
        } else if (cap && said.size() > 3) {
            found = String.format(" → %d answers", said.size());
        } else if (said.size() == 1 && Shapes.ITSELF.equals(said.get(0))) {
            found = String.format(" %s", Shapes.ITSELF);
        } else if (said.size() == 1) {
            found = String.format(" → %s", said.get(0));
        } else {
            found = said.stream()
                .map(Shapes::wrapped)
                .collect(Collectors.joining(" or ", " → ", ""));
        }
        return found;
    }

    private static String wrapped(final String answer) {
        final String found;
        if (answer.contains(" → ")) {
            found = String.format("(%s)", answer);
        } else {
            found = answer;
        }
        return found;
    }
}
