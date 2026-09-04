/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.stream.Collectors;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Put the members of a package inside the object that the package names.
 *
 * <p>The parsed XMIR of the object and of its members are all on disk by the
 * time this runs, and the splice is a matter of moving elements: the
 * top-level {@code <o>} of {@code number/lt.xmir} becomes a child of the
 * top-level {@code <o>} of {@code number.xmir}. No name inside the moved tree
 * is rewritten, because the parser leaves every name fully qualified and every
 * {@code loc} already reads as the locator the node will carry once it is an
 * attribute of {@code Φ.number}, so no reference can be captured by an
 * attribute of the object it lands in.</p>
 *
 * <p>Every package this build compiles an object for is merged, and no other:
 * a package whose name no object carries, as {@code examples} in a program
 * that declares {@code +package examples} and nothing called {@code examples},
 * keeps its members as objects of their own.</p>
 *
 * <p>A member arrives after the attributes the object already had, so the
 * places of the voids, and with them the meaning of applying the object to
 * arguments, stay as they were.</p>
 *
 * <p>Merging twice does nothing the second time: a member that has already
 * moved is marked as merged in the tojos and is not standalone any more, so
 * the package it came from is no longer a package with members and nothing
 * is spliced into the object again. That is what lets {@link MjTranspile}
 * merge on its own without asking whether the {@link MjMerge} goal has
 * already run.</p>
 *
 * <p>The tests a member declares do not travel with it. A test is legal only
 * as a direct child of the top-level object of a file, which is what the
 * parser demands and what the transpiler reads when it writes the test class,
 * so each one is lifted out of the member and appended to the object beside
 * the tests the object declares itself. The member is merged before the
 * package that holds it, so a test written three files deep arrives at the top
 * one level at a time and nothing has to look for it.</p>
 *
 * @since 0.68.0
 */
final class Merging implements Step {

    /**
     * The directory for the merged XMIR.
     */
    static final String DIR = "4-merge";

    /**
     * The tojos of everything this build compiles.
     */
    private final TjsForeign tojos;

    /**
     * The directory to write the merged XMIR to.
     */
    private final Path dir;

    /**
     * Ctor.
     * @param foreign The tojos of everything this build compiles
     * @param target The directory for the merged XMIR
     */
    Merging(final TjsForeign foreign, final Path target) {
        this.tojos = foreign;
        this.dir = target;
    }

    @Override
    public void exec() throws IOException {
        final Map<String, TjForeign> all = this.indexed();
        final Collection<String> found = Merging.deepest(all);
        int done = 0;
        for (final String pkg : found) {
            done = done + this.spliced(pkg, all);
        }
        if (done == 0) {
            Logger.debug(this, "No package member to put inside its object");
        } else {
            Logger.info(
                this, "Put %d member(s) into %d package object(s), XMIR is in %[file]s",
                done, found.size(), this.dir
            );
        }
    }

    private static Collection<String> deepest(final Map<String, TjForeign> all) {
        return all.keySet().stream()
            .filter(name -> name.indexOf('.') > 0)
            .map(name -> name.substring(0, name.lastIndexOf('.')))
            .distinct()
            .filter(all::containsKey)
            .sorted(Merging.deeper())
            .collect(Collectors.toList());
    }

    private static Comparator<String> deeper() {
        return Comparator.comparingInt((String pkg) -> pkg.split("\\.").length)
            .reversed()
            .thenComparing(Comparator.naturalOrder());
    }

    private Map<String, TjForeign> indexed() {
        final Map<String, TjForeign> all = new HashMap<>(0);
        for (final TjForeign tojo : this.tojos.standalone()) {
            all.put(tojo.identifier(), tojo);
        }
        return all;
    }

    private int spliced(final String pkg, final Map<String, TjForeign> all) throws IOException {
        final TjForeign object = all.get(pkg);
        final Map<String, TjForeign> members = Merging.members(pkg, all);
        final Node formation = Merging.formation(object.xmir());
        final Collection<String> taken = Merging.names(formation);
        for (final Map.Entry<String, TjForeign> member : members.entrySet()) {
            final Node top = formation.getOwnerDocument().importNode(
                Merging.top(member.getValue().xmir()).node(), true
            );
            final String name = Merging.named(top);
            if (name.isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "The member '%s' has no name, while only a named object can become an attribute of '%s'",
                        member.getKey(), pkg
                    )
                );
            }
            Merging.claimed(taken, name, member.getKey(), pkg);
            formation.appendChild(top);
            for (final Node test : Merging.tests(top)) {
                Merging.claimed(taken, Merging.named(test), member.getKey(), pkg);
                formation.appendChild(top.removeChild(test));
            }
        }
        final Path target = new Place(pkg).make(this.dir, MjAssemble.XMIR);
        final String merged = new XMLDocument(formation.getOwnerDocument()).toString();
        if (!Files.exists(target) || !new Diff(Files.readString(target), merged).same()) {
            new Saved(merged, target).value();
        }
        object.withXmir(target);
        for (final TjForeign member : members.values()) {
            member.withMerged(pkg);
        }
        Logger.debug(
            this, "Put %d member(s) of '%s' into %[file]s",
            members.size(), pkg, target
        );
        return members.size();
    }

    private static void claimed(
        final Collection<String> taken, final String name, final String member, final String pkg
    ) {
        if (taken.contains(name)) {
            throw new IllegalStateException(
                String.format(
                    "The name '%s' arriving from the member '%s' is already an attribute of '%s', while one object cannot hold two attributes under one name",
                    name, member, pkg
                )
            );
        }
        taken.add(name);
    }

    private static Collection<Node> tests(final Node object) {
        final Collection<Node> found = new ArrayList<>(0);
        final NodeList kids = object.getChildNodes();
        for (int idx = 0; idx < kids.getLength(); ++idx) {
            final Node kid = kids.item(idx);
            final String name = Merging.named(kid);
            if (name.startsWith("+") || name.startsWith("-")) {
                found.add(kid);
            }
        }
        return found;
    }

    private static String named(final Node object) {
        return Optional.ofNullable(object.getAttributes())
            .map(attrs -> attrs.getNamedItem("name"))
            .map(Node::getNodeValue)
            .orElse("");
    }

    private static Map<String, TjForeign> members(
        final String pkg, final Map<String, TjForeign> all
    ) {
        final String prefix = String.format("%s.", pkg);
        final Map<String, TjForeign> found = new TreeMap<>();
        for (final Map.Entry<String, TjForeign> tojo : all.entrySet()) {
            final String name = tojo.getKey();
            if (name.startsWith(prefix) && name.indexOf('.', prefix.length()) < 0) {
                found.put(name, tojo.getValue());
            }
        }
        return found;
    }

    private static Node formation(final Path xmir) throws IOException {
        return Merging.top(xmir).node();
    }

    private static Xnav top(final Path xmir) throws IOException {
        return new Xnav(new XMLDocument(xmir).inner())
            .element("object")
            .element("o");
    }

    private static Collection<String> names(final Node formation) {
        final Collection<String> taken = new ArrayList<>(0);
        new Xnav(formation).elements(Filter.withName("o")).forEach(
            attr -> attr.attribute("name").text().ifPresent(taken::add)
        );
        return taken;
    }
}
