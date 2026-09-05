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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

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
 * nothing in it besides the attributes listed. It is the flag that keeps
 * the checker honest later: a missing attribute is a mistake only when
 * the object that misses it is complete. An atom is not complete, since
 * its {@code λ} attribute stands for a body written in Java, which this
 * module cannot read. Neither is a formation that binds {@code φ}: what
 * it delegates to answers for every name it does not bind itself, and
 * that object is the business of the links table, not of this one.</p>
 *
 * <p>Two kinds of objects are deliberately absent from this table.
 * Applications and references (anything with a {@code @base}) provide
 * nothing on their own — what they have is what the object they copy
 * has, which is the business of the links table. And a void attribute
 * provides nothing either, until something is put into it.</p>
 *
 * <p>What an atom comes back with is written down, though its body is not.
 * {@code [] > div /Q.number} says that a {@code div} is a {@code Φ.number}
 * once it has run, and the parser carries that annotation into the XMIR, so
 * the row keeps it and whoever reads the table can ask a {@code number} what
 * the atom itself cannot answer.</p>
 *
 * <p>An annotation may name a variable rather than an object, and then it
 * names the void that carries the same letter. {@code [] > recovered /A} over
 * {@code ? > value /A?} and {@code ? > alternative /A} says that what comes
 * back is what was put in, so the row keeps the {@code value}, and a caller
 * who put a {@code number} there is answered with a {@code number} rather
 * than with nothing (#8348). The letter stands on both voids, which is the
 * source saying the two are one type; where a caller makes them two, the
 * first is what the table has to go on. A mark of termination is dropped as
 * {@link Held} drops it, since a termination answers to every name.</p>
 *
 * <p>What a void says it will hold is written down for the same reason as an
 * atom's annotation, and only when it names an object. {@code ? > code
 * /Q.number} is how a formation that only Java ever copies says what goes into
 * its voids, since it has no caller in the program to say it for it, while a
 * letter says nothing about what goes in and is read only by the atom above
 * it. {@link Provided} walks through a void that says what it holds the way it
 * walks behind a delegation, so a name asked of it is answered once and for
 * all rather than left to a caller.</p>
 *
 * <p>Not every attribute is written inside the formation it belongs to:
 * {@code minus} in the package {@code number} is {@code Φ.number.minus} and
 * belongs to {@code Φ.number} without ever appearing among its children,
 * which {@link Members} finds. It goes into this table after the attributes
 * a formation binds itself, since the order of those is what binds the
 * arguments of an application and this is not one of them.</p>
 *
 * <p>Nothing is written down about what an object sits in. It was once read
 * off the locator, on the grounds that {@code ρ} is written nowhere and
 * everything has one, and since #6657 neither half holds: a formation says
 * outright what it is dispatched on, and one that says nothing has no
 * {@code ρ} at all for anybody to name.</p>
 *
 * @since 0.67.0
 */
final class Provides implements Clue {

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        final Xmirs world = new Xmirs(xmirs);
        final Collection<XML> made = world.formations();
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            for (final XML formation : made) {
                Provides.fill(rows, new Xnav(formation.inner()));
            }
            new Members(made, world.roots()).fill(rows);
            Files.createDirectories(tables);
            Files.write(
                tables.resolve("provides.xml"),
                new Grouped(rows, "provides").asXml().toString().getBytes(StandardCharsets.UTF_8)
            );
        }
    }

    private static void fill(final Tojos rows, final Xnav shape) {
        final String owner = new Noted(shape).says("loc");
        final List<Xnav> kids = shape.elements(Filter.withName("o")).collect(Collectors.toList());
        rows.add(owner).set("complete", Boolean.toString(Provides.whole(kids)));
        for (final String back : Provides.returns(kids)) {
            rows.add(owner).set("returns", back);
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
                final String held = attr.says("type");
                if (held.startsWith("Φ.")) {
                    row.set("holds", held);
                }
            }
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

    private static String locator(final String annotation, final Collection<Xnav> kids) {
        String found = "";
        if (annotation.startsWith("Φ.")) {
            found = annotation;
        } else if (!annotation.isEmpty()) {
            found = Provides.carrying(kids, annotation);
        }
        return found;
    }

    private static String carrying(final Collection<Xnav> kids, final String letter) {
        String found = "";
        for (final Xnav kid : kids) {
            final Noted attr = new Noted(kid);
            if ("∅".equals(attr.says("base"))
                && letter.equals(attr.says("type").replace("?", ""))) {
                found = attr.says("loc");
                break;
            }
        }
        return found;
    }
}
