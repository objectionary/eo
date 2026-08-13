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
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import org.w3c.dom.Node;

/**
 * Put the members of a package inside the object that the package names.
 *
 * <p>The parsed XMIR of the object and of its members are all on disk by the
 * time this runs, and the splice is a matter of moving one element: the
 * top-level {@code <o>} of {@code number/lt.xmir} becomes a child of the
 * top-level {@code <o>} of {@code number.xmir}. Nothing inside the moved tree
 * is touched, because the parser leaves every name fully qualified and every
 * {@code loc} already reads as the locator the node will carry once it is an
 * attribute of {@code Φ.number}, so no reference can be captured by an
 * attribute of the object it lands in.</p>
 *
 * <p>A member arrives after the attributes the object already had, so the
 * places of the voids, and with them the meaning of applying the object to
 * arguments, stay as they were.</p>
 *
 * @since 0.68.0
 * @todo #6656:30min Write the merged XMIR only when it differs.
 *  The file is written on every build, so its timestamp always moves and
 *  {@link Transpiling} compiles the object again even when neither the object
 *  nor any member of it was touched. It is done this way because the opposite
 *  mistake is worse: a merged object left over from an earlier build would
 *  quietly compile yesterday's member. Comparing the text with what is
 *  already there, and writing only on a difference, would give the
 *  incremental build back without that risk.
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
     * The names of the packages to merge.
     */
    private final Collection<String> packages;

    /**
     * Ctor.
     * @param foreign The tojos of everything this build compiles
     * @param target The directory for the merged XMIR
     * @param names The names of the packages to merge
     */
    Merging(final TjsForeign foreign, final Path target, final Collection<String> names) {
        this.tojos = foreign;
        this.dir = target;
        this.packages = names;
    }

    @Override
    public void exec() throws IOException {
        if (this.packages.isEmpty()) {
            Logger.info(
                this, "No package is named for merging, every member stays an object of its own"
            );
        } else {
            final Map<String, TjForeign> all = this.indexed();
            int done = 0;
            for (final String pkg : this.packages) {
                done = done + this.spliced(pkg, all);
            }
            Logger.info(
                this, "Put %d member(s) into %d package object(s), XMIR is in %[file]s",
                done, this.packages.size(), this.dir
            );
        }
    }

    /**
     * Every compiled object of this build, by its name.
     * @return The tojos, by name
     */
    private Map<String, TjForeign> indexed() {
        final Map<String, TjForeign> all = new HashMap<>(0);
        for (final TjForeign tojo : this.tojos.withXmir()) {
            all.put(tojo.identifier(), tojo);
        }
        return all;
    }

    /**
     * Put every member of one package inside the object it names.
     * @param pkg The name of the package
     * @param all Every compiled object of this build, by its name
     * @return How many members were put inside
     * @throws IOException If the XMIR cannot be read or written
     */
    private int spliced(final String pkg, final Map<String, TjForeign> all) throws IOException {
        final TjForeign object = Optional.ofNullable(all.get(pkg)).orElseThrow(
            () -> new IllegalStateException(
                String.format(
                    "The package '%s' is named for merging, while this build compiles no object '%s' for its members to go into",
                    pkg, pkg
                )
            )
        );
        final Map<String, TjForeign> members = Merging.members(pkg, all);
        final Node formation = Merging.formation(object.xmir());
        final Collection<String> taken = Merging.names(formation);
        for (final Map.Entry<String, TjForeign> member : members.entrySet()) {
            final Xnav top = Merging.top(member.getValue().xmir());
            final String name = top.attribute("name").text().orElseThrow(
                () -> new IllegalStateException(
                    String.format(
                        "The member '%s' has no name, while only a named object can become an attribute of '%s'",
                        member.getKey(), pkg
                    )
                )
            );
            if (taken.contains(name)) {
                throw new IllegalStateException(
                    String.format(
                        "The name '%s' of the member '%s' is already an attribute of '%s', while one object cannot hold two attributes under one name",
                        name, member.getKey(), pkg
                    )
                );
            }
            taken.add(name);
            formation.appendChild(
                formation.getOwnerDocument().importNode(top.node(), true)
            );
        }
        final Path target = new Place(pkg).make(this.dir, MjAssemble.XMIR);
        new Saved(
            new XMLDocument(formation.getOwnerDocument()).toString(),
            target
        ).value();
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

    /**
     * The members of a package, by their names, in the same order every time
     * so that the merged XMIR comes out the same every time too.
     * @param pkg The name of the package
     * @param all Every compiled object of this build, by its name
     * @return The members
     */
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

    /**
     * The top-level object of an XMIR file, as a node that can be moved.
     * @param xmir The path to the XMIR
     * @return The node
     * @throws IOException If the XMIR cannot be read
     */
    private static Node formation(final Path xmir) throws IOException {
        return Merging.top(xmir).node();
    }

    /**
     * The top-level object of an XMIR file.
     * @param xmir The path to the XMIR
     * @return The object
     * @throws IOException If the XMIR cannot be read
     */
    private static Xnav top(final Path xmir) throws IOException {
        return new Xnav(new XMLDocument(xmir).inner())
            .element("object")
            .element("o");
    }

    /**
     * The names of the attributes an object already holds.
     * @param formation The top-level object
     * @return The names, in a collection that can be added to
     */
    private static Collection<String> names(final Node formation) {
        final Collection<String> taken = new ArrayList<>(0);
        new Xnav(formation).elements(Filter.withName("o")).forEach(
            attr -> attr.attribute("name").text().ifPresent(taken::add)
        );
        return taken;
    }
}
