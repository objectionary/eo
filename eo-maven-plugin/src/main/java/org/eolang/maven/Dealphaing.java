/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import io.github.sekator778.seqdir.Seqdir;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import org.eolang.cache.Saved;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Replace the positional names of arguments with the names of their voids.
 *
 * <p>The XMIR of every object this build compiles is read from wherever its
 * tojo points, and every {@code @as} of the {@code αN} form is replaced with
 * the name {@link MjDealpha} read for the {@code @loc} of the argument. The
 * {@code @loc} stays as it is, so the rows of the inference tables still
 * point at the argument. The result goes to a directory {@code Seqdir}
 * numbers "dealpha" and the tojo points there, which is where
 * {@link MjTranspile} reads it from.</p>
 *
 * <p>No object is treated apart: the bytes of a literal are named after the
 * {@code φ} of {@code Φ.number} like any other argument, and the branches of
 * an {@code .if} after the voids of the {@code if} they reach. Whatever reads
 * an argument afterwards counts its place among the arguments instead of
 * reading it off the name.</p>
 *
 * <p>An argument with no name to take keeps its place, and is
 * counted. The arguments of one application are named whole: when one of
 * them has no name, none of them takes one, and every one is counted. With
 * the strict flag set, the build fails when there is one, after every file is
 * written and the numbers are in the log.</p>
 *
 * <p>The arguments of <code>&#x22A5;</code> are not judged at all: <code>&#x22A5;</code> has no
 * voids, so its message has no name to take. It keeps its {@code αN} and is
 * counted apart, as a message of <code>&#x22A5;</code>, not as lost. The objects inside
 * the message are still walked, so the arguments of a call that makes the
 * message are named as usual.</p>
 *
 * @since 0.69.0
 */
final class Dealphaing {

    /**
     * The positional name of an argument.
     */
    private static final Pattern ALPHA = Pattern.compile("^α[0-9]+$");

    /**
     * The tojos of the objects to rename arguments in.
     */
    private final Collection<TjForeign> tojos;

    /**
     * The names of the voids the arguments land in, by their locators.
     */
    private final Map<String, String> names;

    /**
     * Base target directory.
     */
    private final Path target;

    /**
     * Whether to fail when an argument has no name to take.
     */
    private final boolean strict;

    /**
     * Ctor.
     *
     * @param objects The tojos of the objects to rename arguments in
     * @param lands The names of the voids the arguments land in, by their
     *  locators
     * @param tgt Base target directory
     * @param fail Whether to fail when an argument has no name to take
     */
    Dealphaing(
        final Collection<TjForeign> objects,
        final Map<String, String> lands,
        final Path tgt,
        final boolean fail
    ) {
        this.tojos = objects;
        this.names = lands;
        this.target = tgt;
        this.strict = fail;
    }

    /**
     * Rename positional arguments in every XMIR.
     *
     * @throws IOException If XMIR cannot be read or written
     */
    void exec() throws IOException {
        final Path dir = new Seqdir(this.target, 2).dirs().once("dealpha");
        final Map<String, Collection<String>> verdicts = new HashMap<>(4);
        for (final TjForeign tojo : this.tojos) {
            Dealphaing.renamed(tojo, dir, this.names, verdicts);
        }
        final Collection<String> lost = verdicts.getOrDefault("lost", new ArrayList<>(0));
        Logger.info(
            this,
            "Named %d of %d positional argument(s) in %d XMIR(s), %d are messages of ⊥, %d found no void to be named after",
            verdicts.getOrDefault("named", new ArrayList<>(0)).size(),
            verdicts.values().stream().mapToInt(Collection::size).sum(),
            this.tojos.size(),
            verdicts.getOrDefault("bottom", new ArrayList<>(0)).size(),
            lost.size()
        );
        try (Stream<Path> found = Files.walk(dir)) {
            Logger.info(
                this,
                "%d XMIR files are in %[file]s",
                found
                    .filter(path -> path.toString().endsWith(".xmir"))
                    .filter(Files::isRegularFile)
                    .count(),
                dir
            );
        }
        if (this.strict && !lost.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "%d positional argument(s) found no void to be named after, such as '%s', while eo.dealpha.rigid demands a name for every one",
                    lost.size(), lost.iterator().next()
                )
            );
        }
    }

    private static void renamed(
        final TjForeign tojo,
        final Path dir,
        final Map<String, String> names,
        final Map<String, Collection<String>> verdicts
    ) throws IOException {
        final Node xmir = new XMLDocument(tojo.xmir()).inner();
        Dealphaing.walked(xmir, names, verdicts);
        final Path dest = new Place(tojo.identifier()).make(dir, MjAssemble.XMIR);
        final String named = new XMLDocument(xmir).toString();
        if (!Files.exists(dest) || !new Diff(Files.readString(dest), named).same()) {
            new Saved(named, dest).value();
        }
        tojo.withXmir(dest);
    }

    private static void walked(
        final Node node,
        final Map<String, String> names,
        final Map<String, Collection<String>> verdicts
    ) {
        final NodeList kids = node.getChildNodes();
        final Collection<Node> args = new ArrayList<>(kids.getLength());
        for (int idx = 0; idx < kids.getLength(); ++idx) {
            final Node kid = kids.item(idx);
            if ("o".equals(kid.getNodeName())) {
                final Node alias = kid.getAttributes().getNamedItem("as");
                if (alias != null && Dealphaing.ALPHA.matcher(alias.getNodeValue()).matches()) {
                    args.add(kid);
                }
            }
        }
        if ("⊥".equals(Dealphaing.attr(node, "base"))) {
            for (final Node arg : args) {
                verdicts.computeIfAbsent("bottom", key -> new ArrayList<>(1))
                    .add(Dealphaing.attr(arg, "loc"));
            }
        } else {
            Dealphaing.judged(args, names, verdicts);
        }
        for (int idx = 0; idx < kids.getLength(); ++idx) {
            Dealphaing.walked(kids.item(idx), names, verdicts);
        }
    }

    private static void judged(
        final Collection<Node> args,
        final Map<String, String> names,
        final Map<String, Collection<String>> verdicts
    ) {
        final boolean lands = args.stream().allMatch(
            arg -> names.containsKey(Dealphaing.attr(arg, "loc"))
        );
        final String verdict;
        if (lands) {
            verdict = "named";
        } else {
            verdict = "lost";
        }
        for (final Node arg : args) {
            final String loc = Dealphaing.attr(arg, "loc");
            if (lands) {
                arg.getAttributes().getNamedItem("as").setNodeValue(names.get(loc));
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
