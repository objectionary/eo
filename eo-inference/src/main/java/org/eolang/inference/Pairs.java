/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import org.w3c.dom.Node;

/**
 * What the links table says, row by row.
 *
 * <p>{@link Links} writes one row per name that is a copy of another, and
 * reading them back is how a later pass adds to them without losing what is
 * there. The order they were written in is kept, so a document read and written
 * again keeps the rules' rows where they were and carries the worked-out ones
 * after them.</p>
 *
 * <p>This is the only place the table is read, and it is read by walking it
 * rather than by asking it questions. A table of a program of any size is a
 * document of megabytes, and asking such a document a question costs the same
 * whether one row comes back or forty thousand do, since the whole of it is
 * looked through either way. Every question here is about all the rows anyway,
 * so one walk answers all of them, and the rows of that walk are kept for
 * whoever asks next.</p>
 *
 * @since 0.68.0
 */
final class Pairs {

    /**
     * The links table.
     */
    private final XML table;

    /**
     * The rows of the table, once they have been walked.
     */
    private final List<List<Xnav>> read;

    /**
     * Ctor.
     * @param links The links table
     */
    Pairs(final XML links) {
        this.table = links;
        this.read = new ArrayList<>(1);
    }

    /**
     * Every pair of the table.
     *
     * <p>A row whose type is not an object of the program is passed over: it
     * says something true, and nothing that a chain of copies can be walked
     * through.</p>
     *
     * @return The pairs, each name against the one it is a copy of
     */
    Map<String, String> all() {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Xnav row : this.rows()) {
            final Optional<Xnav> ref = Pairs.ref(row);
            if (ref.isPresent()) {
                found.put(new Noted(row).says("id"), new Noted(ref.get()).says("loc"));
            }
        }
        return found;
    }

    /**
     * Every object the table answers by itself.
     *
     * <p>A row holding a reference is an answer only once the reference has
     * been followed, and where it leads may be a void or nothing at all. A
     * datum and a termination are answers as they stand: the bytes of a
     * literal are what they are, and an object that never comes back with a
     * value has nothing further to say.</p>
     *
     * @return The locators
     */
    Collection<String> certain() {
        final Collection<String> found = new ArrayList<>(0);
        for (final Xnav row : this.rows()) {
            final String form = Pairs.form(row);
            if ("data".equals(form) || "terminator".equals(form)) {
                found.add(new Noted(row).says("id"));
            }
        }
        return found;
    }

    /**
     * Which form of answer every row of the table holds.
     *
     * <p>A row holds one element and the element says what kind of answer it
     * is, so its name is the form: a {@code data}, a {@code terminator}, a
     * {@code var}, a {@code ref} to the object this one is a copy of, or
     * whatever a pass we know nothing about has written there.</p>
     *
     * @return The form, by the locator of the object the row is about
     */
    Map<String, String> forms() {
        final Map<String, String> found = new HashMap<>(0);
        for (final Xnav row : this.rows()) {
            found.put(new Noted(row).says("id"), Pairs.form(row));
        }
        return found;
    }

    /**
     * Every row of the table that is not a pair.
     *
     * <p>A pass that reads the table and writes it again can only build the
     * kinds of answer it knows about, and it knows about pairs. Everything
     * else comes back as {@link Kept}, to be written as it was found rather
     * than dropped for being none of that pass's business.</p>
     *
     * @return The types, by the locator of the object they are about
     */
    Map<String, Type> others() {
        final Map<String, Type> found = new LinkedHashMap<>(0);
        for (final Xnav row : this.rows()) {
            if (!Pairs.ref(row).isPresent()) {
                found.put(new Noted(row).says("id"), new Kept(row));
            }
        }
        return found;
    }

    /**
     * Every void an object of the table has filled.
     *
     * <p>The binds of one row are not the whole of it. A copy of a copy keeps
     * what the earlier copy put in: {@code pair u > half} fills one void and
     * {@code half v > full} the other, and {@code full} holds both, though its
     * own row names only the second. So the chain is walked and the binds
     * found along it are gathered together.</p>
     *
     * @return The locators of the voids filled, by the locator of the object
     *  that filled them, without the objects that filled none
     */
    Map<String, Collection<String>> filled() {
        final Map<String, String> hops = new LinkedHashMap<>(0);
        final Map<String, Collection<String>> own = new LinkedHashMap<>(0);
        for (final Xnav row : this.rows()) {
            final Optional<Xnav> ref = Pairs.ref(row);
            if (ref.isPresent()) {
                final String object = new Noted(row).says("id");
                hops.put(object, new Noted(ref.get()).says("loc"));
                final Collection<String> voids = ref.get()
                    .elements(Filter.withName("bind"))
                    .map(bind -> new Noted(bind).says("void"))
                    .collect(Collectors.toList());
                if (!voids.isEmpty()) {
                    own.put(object, voids);
                }
            }
        }
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        for (final String object : hops.keySet()) {
            final Collection<String> voids = new LinkedHashSet<>(0);
            final Collection<String> seen = new HashSet<>(0);
            String walked = object;
            while (seen.add(walked)) {
                voids.addAll(own.getOrDefault(walked, Collections.emptyList()));
                if (!hops.containsKey(walked)) {
                    break;
                }
                walked = hops.get(walked);
            }
            if (!voids.isEmpty()) {
                found.put(object, voids);
            }
        }
        return found;
    }

    /**
     * What the table says went into every void.
     *
     * <p>A bind of a row names a void and what was put into it, and the same
     * void is named by the row of every copy that filled it. What went in is
     * gathered per void and not per row, since a void filled with a
     * {@code number} at eleven call sites was filled one way eleven times.</p>
     *
     * @return The locators of what went in, by the locator of the void, in the
     *  order the table names them, without the binds that put nothing
     */
    Map<String, Collection<String>> puts() {
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        for (final Xnav row : this.rows()) {
            final Optional<Xnav> ref = Pairs.ref(row);
            if (ref.isPresent()) {
                ref.get().elements(Filter.withName("bind")).forEach(
                    bind -> Pairs.ref(bind).ifPresent(
                        put -> found.computeIfAbsent(
                            new Noted(bind).says("void"), key -> new LinkedHashSet<>(0)
                        ).add(new Noted(put).says("loc"))
                    )
                );
            }
        }
        return found;
    }

    /**
     * The reference of every pair of the table, as the document holds it.
     *
     * <p>A pass that adds to a reference writes into the document itself
     * rather than building a new one, so what comes back is the node the table
     * keeps and not a copy of it.</p>
     *
     * @return The references, by the locator of the object they are about
     */
    Map<String, Node> refs() {
        final Map<String, Node> found = new LinkedHashMap<>(0);
        for (final Xnav row : this.rows()) {
            final Optional<Xnav> ref = Pairs.ref(row);
            if (ref.isPresent()) {
                found.putIfAbsent(new Noted(row).says("id"), ref.get().node());
            }
        }
        return found;
    }

    private List<Xnav> rows() {
        if (this.read.isEmpty()) {
            this.read.add(new Rows(this.table).all());
        }
        return this.read.get(0);
    }

    private static Optional<Xnav> ref(final Xnav node) {
        return node.elements(Filter.withName("ref")).findFirst();
    }

    private static String form(final Xnav row) {
        return row.elements()
            .map(Xnav::node)
            .filter(held -> held.getNodeType() == Node.ELEMENT_NODE)
            .map(Node::getNodeName)
            .findFirst()
            .orElse("");
    }
}
