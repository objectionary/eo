/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjDeferred;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.xembly.Directives;

/**
 * What every object certainly has.
 *
 * <p>This is the first and the simplest rule of the checker: a formation
 * shows its attributes right in the code, so we can write them down
 * without thinking. This formation</p>
 *
 * <pre> [] &gt; t
 *   [] &gt; next</pre>
 *
 * <p>fills the table with three rows. One says that {@code Φ.t} is
 * complete, another that {@code Φ.t} has an attribute {@code next} whose
 * type is {@code Φ.t.next}, and the third that {@code Φ.t.next} is
 * complete as well, and has nothing.</p>
 *
 * <p>So a row is either about a type or about one of its attributes, and
 * which one it is can be seen from its cells: an attribute row carries
 * an {@code owner}. A type is identified by its locator, an attribute by
 * the locator of its owner and its own name, because those two are what
 * make it unique — its own locator would not do, since an attribute is
 * also a type in its own right, and the two rows would collide.</p>
 *
 * <p>"Complete" means that we have seen the whole formation, so there is
 * nothing in it besides the attributes listed. {@link Reads} reads a name a
 * formation lacks off its {@code φ}, or off what its atom returns, and
 * answers it with a termination only when the formation is complete. An
 * atom is not complete, since its {@code λ} attribute stands for a body
 * written in Java, which this module cannot read. Neither is a formation
 * that binds {@code φ}: what it delegates to answers for every name it does
 * not bind itself.</p>
 *
 * <p>Two kinds of objects are deliberately absent from this table.
 * Applications and references (anything with a {@code @base}) provide
 * nothing on their own: what they have is what the object they copy has,
 * which the walk works out from the heads, the reads and the calls. And a
 * void attribute provides nothing either, until something is put into
 * it.</p>
 *
 * <p>What an atom comes back with is written down, though its body is not.
 * {@code [] > div /Q.number} says that a {@code div} is a {@code Φ.number}
 * once it has run, and the parser carries that annotation into the XMIR, so
 * the row keeps it and whoever reads the table can ask a {@code number} what
 * the atom itself cannot answer.</p>
 *
 * <p>An annotation is a type tree ({@link Annotation}):
 * {@code /Q.posix.status-return(Q.number,Q.string)} gives a
 * {@code status-return} with a number and a string in its voids. The
 * {@code returns} cell keeps the tree as the XMIR writes it, and the
 * {@code holds} cell of a void keeps its own type when it names an object,
 * without the mark of termination.</p>
 *
 * <p>An annotation may name a variable rather than an object, and then it
 * names the voids that carry the same letter. {@code [] > recovered /A} over
 * {@code ? > value /A?} and {@code ? > alternative /A} says that what comes
 * back is what was put in, so the row keeps the {@code value}, and a caller
 * who put a {@code number} there is answered with a {@code number} rather
 * than with nothing (#8348). The letter stands on both voids, so what comes
 * back is the {@code value} or the {@code alternative}, and the facts name
 * every void that carries it, a {@code return} element each under the row of
 * the atom. The {@code returns} cell keeps the first alone, which is enough
 * for {@link Roles} to tell an atom, and {@link Roles} and {@link Telling}
 * read the {@code return} elements for the whole list. A mark of termination
 * is dropped, since a termination answers to every name.</p>
 *
 * <p>What a void says it will hold is written down for the same reason as an
 * atom's annotation, and only when it names an object. {@code ? > code
 * /Q.number} is how a formation that only Java ever copies says what goes into
 * its voids, since it has no caller in the program to say it for it, while a
 * letter says nothing about what goes in and is read only by the atom above
 * it. {@link Reads} answers a read of such a void with a copy of what it
 * holds, next to whatever the calls put there, so a name asked of it has an
 * answer in every copy.</p>
 *
 * <p>A void may also say what will be handed to whatever goes into it.
 * {@code ? > scope /{Q.chunk}} in {@code malloc.of} says that the atom calls
 * its {@code scope} with a chunk, which is what {@code EOmalloc$EOof} then
 * does, and the row keeps that list as it stands. Nobody else in the program
 * says it: the formation {@code malloc.for} hands in is copied by Java alone,
 * so without the annotation its void is filled by nobody and looks empty to
 * every reader (#8380). {@link Calls} spends the list: whatever a call puts
 * into such a void is called back with copies of those types.</p>
 *
 * <p>Not every attribute is written inside the formation it belongs to:
 * {@code minus} in the package {@code number} is {@code Φ.number.minus} and
 * belongs to {@code Φ.number} without ever appearing among its children,
 * which {@link Packages} finds. It goes into this table after the attributes
 * a formation binds itself, since the order of those is what binds the
 * arguments of an application and this is not one of them.</p>
 *
 * <p>A type is "bare" when it has nothing public of its own besides its
 * {@code @}: no attribute other than {@code φ} that is not a void and has no
 * {@code 🌵} in its name. A {@code 🌵} name is kept out of the {@code NAME}
 * token by R-9.2.2 of the parser spec and no source file can write one down,
 * which covers an auto-name and a test attribute alike, the latter under its
 * {@code p🌵} or {@code n🌵} marker. The flag is decided last, once
 * {@link Packages} has written what the packages hand to their objects, since
 * a member is as public as an attribute written inside the formation:
 * {@code [@] > number} binds nothing but a body in its own file, and is not
 * bare for all that, because {@code abs} and {@code plus} sit in its package.
 * The engine does not read the flag: {@link Roles} tells a decorator from the
 * attributes {@link Formation} keeps.</p>
 *
 * <p>A package is an object as well, though no file forms it: the runtime
 * hands out a package object for a folder that has no object of its own.
 * {@link Packages} gives every such package a row, complete, with the next
 * level under it as its attributes, and the global object {@code Φ} is the
 * package at the top, the one every chain of names off a bare {@code Φ} is
 * read from. None of them is bare, having names of their own.</p>
 *
 * <p>Nothing is written down about what an object sits in. It was once read
 * off the locator, on the grounds that {@code ρ} is written nowhere and
 * everything has one, and since #6657 neither half holds: a formation says
 * outright what it is dispatched on, and one that says nothing has no
 * {@code ρ} at all for anybody to name.</p>
 *
 * <p>What goes into the {@code ρ} a formation does declare is written down,
 * which is a different claim and {@link Received}'s: an attribute of an
 * {@code oak} is reached by taking it off an {@code oak}, so the void holds
 * one. Only a formation that asked for a receiver is told what it gets, and
 * the row was already there for the asking. It goes in as the {@code rho}
 * of the void, the formation a declared {@code ^} sits in, rather than as
 * what it {@code holds}, since nobody wrote it down.</p>
 *
 * <p>Every row here is stated by the text, so the rows are facts and go into
 * the {@code objects} section of {@code facts.xml}. {@link FactsStream} reads
 * each {@code type} row back as a {@link Formation}, which the walk asks what
 * an object has. The walk writes its answers into {@code answers.xml} and
 * adds nothing to these rows.</p>
 *
 * @since 0.67.0
 */
final class Provides implements Section {

    @Override
    public Directives directives(final Path xmirs) throws IOException {
        final Xmirs world = new Xmirs(xmirs);
        final Collection<XML> made = world.formations();
        final Directives slots = new Directives();
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            for (final XML formation : made) {
                Provides.fill(rows, slots, new Xnav(formation.inner()));
            }
            new Packages(world.owners(), world.roots()).fill(rows);
            Provides.mark(rows);
            return new Grouped(rows, "objects").directives()
                .append(slots)
                .append(new Received(made).directives(rows));
        }
    }

    private static void fill(final Tojos rows, final Directives slots, final Xnav shape) {
        final String owner = new Noted(shape).says("loc");
        final List<Xnav> kids = shape.elements(Filter.withName("o")).collect(Collectors.toList());
        rows.add(owner).set("complete", Boolean.toString(Provides.whole(kids)));
        for (final String back : Provides.returns(kids)) {
            rows.add(owner).set("returns", back);
        }
        for (final String slot : Provides.slots(kids)) {
            slots.xpath(String.format("objects/type[@id='%s']", owner))
                .add("return")
                .attr("loc", slot)
                .up()
                .up()
                .up();
        }
        for (final Xnav kid : Provides.named(kids)) {
            final Noted attr = new Noted(kid);
            final String name = attr.says("name");
            final Tojo row = rows.add(String.join(" ", owner, name))
                .set("owner", owner)
                .set("name", name)
                .set("type", attr.says("loc"));
            if ("∅".equals(attr.says("base"))) {
                row.set("void", "true");
                final String held = Provides.held(attr.says("type"));
                if (!held.isEmpty()) {
                    row.set("holds", held);
                }
                final String args = attr.says("args");
                if (!args.isEmpty()) {
                    row.set("args", args);
                }
            }
        }
    }

    private static void mark(final Tojos rows) {
        final Collection<String> kept = new HashSet<>(0);
        for (final Tojo row : rows.select(one -> one.exists("owner"))) {
            final Map<String, String> cells = row.toMap();
            final String name = cells.getOrDefault("name", "");
            if (!name.isEmpty() && !"φ".equals(name)
                && !"true".equals(cells.get("void")) && !name.contains("🌵")) {
                kept.add(cells.get("owner"));
            }
        }
        for (final Tojo type : rows.select(one -> !one.exists("owner"))) {
            type.set("bare", Boolean.toString(!kept.contains(type.get("id"))));
        }
    }

    private static Collection<Xnav> named(final Collection<Xnav> kids) {
        final Collection<Xnav> found = new ArrayList<>(0);
        for (final Xnav kid : kids) {
            final String name = new Noted(kid).says("name");
            if (!name.isEmpty() && !"λ".equals(name)) {
                found.add(kid);
            }
        }
        return found;
    }

    private static boolean whole(final Collection<Xnav> kids) {
        boolean found = true;
        for (final Xnav kid : kids) {
            final String name = new Noted(kid).says("name");
            if ("λ".equals(name) || "φ".equals(name)) {
                found = false;
                break;
            }
        }
        return found;
    }

    private static Collection<String> returns(final Collection<Xnav> kids) {
        final Collection<String> found = new ArrayList<>(0);
        for (final Xnav kid : kids) {
            final Noted attr = new Noted(kid);
            if ("λ".equals(attr.says("name"))) {
                final String back = Provides.locator(attr.says("atom"), kids);
                if (!back.isEmpty()) {
                    found.add(back);
                }
            }
        }
        return found;
    }

    private static Collection<String> slots(final Collection<Xnav> kids) {
        final Collection<String> found = new ArrayList<>(0);
        for (final Xnav kid : kids) {
            final Noted attr = new Noted(kid);
            final String annotation = attr.says("atom");
            if ("λ".equals(attr.says("name"))
                && !annotation.isEmpty() && !annotation.startsWith("Φ.")) {
                found.addAll(Provides.carrying(kids, annotation));
            }
        }
        return found;
    }

    private static String locator(final String annotation, final Collection<Xnav> kids) {
        String found = "";
        if (annotation.startsWith("Φ.")) {
            found = annotation;
        } else if (!annotation.isEmpty()) {
            found = Provides.carrying(kids, annotation).stream().findFirst().orElse("");
        }
        return found;
    }

    private static List<String> carrying(final Collection<Xnav> kids, final String letter) {
        final List<String> found = new ArrayList<>(0);
        for (final Xnav kid : kids) {
            final Noted attr = new Noted(kid);
            if ("∅".equals(attr.says("base"))
                && letter.equals(attr.says("type").replace("?", ""))) {
                found.add(attr.says("loc"));
            }
        }
        return found;
    }

    private static String held(final String type) {
        final String bare = new Annotation(type).toString();
        String found = "";
        if (bare.startsWith("Φ.")) {
            found = bare;
        }
        return found;
    }
}
