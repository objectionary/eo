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
 * The formations and their declared attributes, as the program writes them.
 *
 * <p>The rows go into the {@code objects} section of {@code facts.xml}.
 * A formation has a {@code type} row identified by its locator, and every
 * named child has an {@code attr} row with its name and locator, in source
 * order. This keeps the order in which positional arguments fill voids.
 * Applications, references and voids have no formation row of their own.</p>
 *
 * <p>The {@code complete} flag says that the formation has neither an atom
 * nor a bound {@code φ}. An atom's Java body is not visible here, and a
 * decorator may answer a missing name through its {@code φ}; the engine
 * works out those answers rather than this section guessing them.</p>
 *
 * <p>An atom's declared return object is kept as {@code returns}. A return
 * annotation that names a type variable instead keeps the locators of the
 * voids carrying that variable in {@code return} elements, with the first
 * also stored as {@code returns}. A termination annotation contributes no
 * return locator.</p>
 *
 * <p>A void is marked {@code void="true"}. Its annotation is kept as
 * {@code holds} when it names an object, and its callback argument list as
 * {@code args}, when present. These describe what Java puts into the void
 * or passes to a callback. {@link Received} adds the {@code rho} locator
 * for a declared receiver, separately from the source's {@code holds}.</p>
 *
 * <p>{@link Packages} adds members declared in separate files and synthetic
 * package objects, including the global object {@code Φ}. Only after those
 * attributes are present is each type marked {@code bare}: it has no
 * non-void attribute other than {@code φ} whose name lacks a cactus.
 * Thus package members count, while voids and compiler-generated names do
 * not. This flag describes the declarations; it does not reduce the object
 * to what its {@code φ} returns.</p>
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
                final String held = attr.says("type");
                if (held.startsWith("Φ.")) {
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
}
