/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Replace the positional names of arguments with the names of their voids.
 *
 * <p>The XMIR of every object this build compiles is read from wherever its
 * tojo points, and every {@code @as} of the {@code αN} form is replaced with
 * the name {@link Landings} knows for the {@code @loc} of the argument. The
 * {@code @loc} stays as it is, so the rows of the inference tables still
 * point at the argument. The result goes to {@link #DIR} and the tojo points
 * there, which is where {@link MjTranspile} reads it from.</p>
 *
 * <p>No object is treated apart: the bytes of a literal are named after the
 * {@code φ} of {@code Φ.number} like any other argument, and the branches of
 * an {@code .if} after the voids of the {@code if} they reach. Whatever reads
 * an argument afterwards counts its place among the arguments instead of
 * reading it off the name.</p>
 *
 * <p>An argument with no name to take keeps its place, and is
 * counted. With the strict flag set, the build fails when there is one, after
 * every file is written and the numbers are in the log.</p>
 *
 * @since 0.69.0
 * @todo #8301:90min Keep the purity of formations passed as named arguments.
 *  The transpiler runs set-locators.xsl again, so a renamed argument gets a
 *  new locator, while purify.xsl looks formations up in the inference tables
 *  by the old one. A formation inside a renamed argument is never marked as
 *  pure because of that. Let set-locators.xsl keep the locator of an argument
 *  the way the parser gave it, or let purify.xsl read the original locator.
 */
final class Dealphaing implements Step {

    /**
     * The directory for the XMIR with named arguments.
     */
    static final String DIR = "7-dealpha";

    /**
     * The positional name of an argument.
     */
    private static final Pattern ALPHA = Pattern.compile("^α[0-9]+$");

    /**
     * The tojos of the objects to rename arguments in.
     */
    private final Collection<TjForeign> tojos;

    /**
     * The names of the voids the arguments land in.
     */
    private final Landings landings;

    /**
     * The directory to write the XMIR to.
     */
    private final Path dir;

    /**
     * Whether to fail when an argument has no name to take.
     */
    private final boolean strict;

    /**
     * Ctor.
     *
     * @param objects The tojos of the objects to rename arguments in
     * @param names The names of the voids the arguments land in
     * @param target The directory to write the XMIR to
     * @param fail Whether to fail when an argument has no name to take
     */
    Dealphaing(
        final Collection<TjForeign> objects,
        final Landings names,
        final Path target,
        final boolean fail
    ) {
        this.tojos = objects;
        this.landings = names;
        this.dir = target;
        this.strict = fail;
    }

    @Override
    public void exec() throws IOException {
        if (this.tojos.isEmpty()) {
            Logger.debug(this, "No XMIR to name the arguments in");
        } else {
            final Map<String, String> names = this.landings.names();
            final Map<String, Collection<String>> verdicts = new HashMap<>(3);
            for (final TjForeign tojo : this.tojos) {
                this.renamed(tojo, names, verdicts);
            }
            final Collection<String> lost = verdicts.getOrDefault("lost", new ArrayList<>(0));
            Logger.info(
                this,
                "Named %d of %d positional argument(s) in %d XMIR(s), %d found no void to be named after, XMIR is in %[file]s",
                verdicts.getOrDefault("named", new ArrayList<>(0)).size(),
                verdicts.values().stream().mapToInt(Collection::size).sum(),
                this.tojos.size(),
                lost.size(),
                this.dir
            );
            if (this.strict && !lost.isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "%d positional argument(s) found no void to be named after, such as '%s', while eo.failOnAlpha demands a name for every one",
                        lost.size(), lost.iterator().next()
                    )
                );
            }
        }
    }

    private void renamed(
        final TjForeign tojo,
        final Map<String, String> names,
        final Map<String, Collection<String>> verdicts
    ) throws IOException {
        final Node xmir = new XMLDocument(tojo.xmir()).inner();
        Dealphaing.walked(xmir, names, verdicts);
        final Path target = new Place(tojo.identifier()).make(this.dir, MjAssemble.XMIR);
        final String named = new XMLDocument(xmir).toString();
        if (!Files.exists(target) || !new Diff(Files.readString(target), named).same()) {
            new Saved(named, target).value();
        }
        tojo.withXmir(target);
    }

    private static void walked(
        final Node node,
        final Map<String, String> names,
        final Map<String, Collection<String>> verdicts
    ) {
        final NodeList kids = node.getChildNodes();
        for (int idx = 0; idx < kids.getLength(); ++idx) {
            final Node kid = kids.item(idx);
            if ("o".equals(kid.getNodeName())) {
                Dealphaing.judged(kid, names, verdicts);
            }
            Dealphaing.walked(kid, names, verdicts);
        }
    }

    private static void judged(
        final Node arg,
        final Map<String, String> names,
        final Map<String, Collection<String>> verdicts
    ) {
        final Node alias = arg.getAttributes().getNamedItem("as");
        if (alias != null && Dealphaing.ALPHA.matcher(alias.getNodeValue()).matches()) {
            final String loc = Dealphaing.attr(arg, "loc");
            final String verdict;
            if (names.containsKey(loc)) {
                alias.setNodeValue(names.get(loc));
                verdict = "named";
            } else {
                verdict = "lost";
            }
            verdicts.computeIfAbsent(verdict, key -> new ArrayList<>(1)).add(loc);
        }
    }

    private static String attr(final Node node, final String name) {
        return Optional.ofNullable(node.getAttributes())
            .map(attrs -> attrs.getNamedItem(name))
            .map(Node::getNodeValue)
            .orElse("");
    }
}
