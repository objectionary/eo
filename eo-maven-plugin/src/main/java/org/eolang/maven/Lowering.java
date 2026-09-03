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
import java.util.List;
import java.util.stream.Collectors;
import org.eolang.lowering.Constant;
import org.eolang.lowering.Datum;
import org.eolang.lowering.Phino;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Fold the constant fragments of every XMIR this build compiles.
 *
 * <p>A fragment qualifies when it is an application whose every leaf is
 * a literal, such as {@code 1.plus 1}: its value is decided by data
 * alone, so it is computed here, once, through {@code phino dataize},
 * and a literal of that value stands where the fragment stood. Which
 * methods can fold is phino's knowledge, not ours: every qualifying
 * fragment is simply tried, and one that dispatches outside the universe
 * fails there. Such a fragment is pure by construction — a literal
 * receiver dispatches into the standard library and nowhere else — so no
 * purity analysis is consulted. The walk is top-down and the outermost
 * qualifying fragment wins, since folding it folds everything inside
 * it.</p>
 *
 * <p>Folding is best-effort per fragment: whatever phino refuses — an
 * error-path expression, a method outside its tables, an exhausted
 * budget — stays as written, and the build goes on. A file with nothing
 * folded is neither rewritten nor repointed, so a build without foldable
 * fragments leaves no trace of this step at all.</p>
 *
 * <p>Before the constants, {@link Lowered} rewrites the pure formations
 * whose value symbolic reduction can compute: their bodies become sidecar
 * Java files, and the formations keep only their voids and a {@code λ}
 * marker, so {@code lowered.xsl} later renders each one as an atom class.
 * The same best-effort rule holds there: a formation that refuses stays
 * as written.</p>
 *
 * @since 0.76.0
 */
final class Lowering implements Step {

    /**
     * The directory for the folded XMIR.
     */
    static final String DIR = "4-lower";

    /**
     * The file that says lowering ran, and with what.
     */
    static final String MARKER = "lowering.txt";

    /**
     * The subdirectory of {@link #DIR} with the sidecar bodies of the
     * lowered formations, one {@code <digest>.java} per distinct fragment,
     * spliced into generated atom classes by {@code lowered.xsl}.
     */
    static final String ATOMS = "atoms";

    /**
     * XMIR sources to fold.
     */
    private final Collection<TjForeign> sources;

    /**
     * The directory to write the folded XMIR to.
     */
    private final Path home;

    /**
     * The binary that dataizes.
     */
    private final Phino phino;

    /**
     * The rewriter of pure formations into synthetic atoms.
     */
    private final Lowered riser;

    /**
     * Ctor.
     * @param srcs XMIR sources to fold
     * @param target The directory for the folded XMIR
     * @param exe The binary that dataizes
     * @param tables The directory with the tables of {@link MjInference}
     */
    Lowering(final Collection<TjForeign> srcs, final Path target,
        final Phino exe, final Path tables) {
        this(
            srcs, target, exe,
            new Lowered(exe, tables, target.resolve(Lowering.ATOMS))
        );
    }

    /**
     * Ctor.
     * @param srcs XMIR sources to fold
     * @param target The directory for the folded XMIR
     * @param exe The binary that dataizes
     * @param atoms The rewriter of pure formations into synthetic atoms
     */
    Lowering(final Collection<TjForeign> srcs, final Path target,
        final Phino exe, final Lowered atoms) {
        this.sources = srcs;
        this.home = target;
        this.phino = exe;
        this.riser = atoms;
    }

    @Override
    public void exec() throws IOException {
        Logger.info(
            this, "Folded or lowered %d fragment(s) in %d XMIR(s), into %[file]s",
            new Threaded<>(this.sources, this::folded).total(),
            this.sources.size(), this.home
        );
    }

    private int folded(final TjForeign tojo) throws IOException {
        final XMLDocument doc = new XMLDocument(tojo.xmir());
        int count = this.riser.rewrite(doc);
        final Collection<Xnav> found = new ArrayList<>(0);
        Lowering.selected(
            new Xnav(doc.inner()).element("object").element("o"),
            found
        );
        for (final Xnav node : found) {
            if (this.spliced(node)) {
                ++count;
            }
        }
        if (count > 0) {
            final Path target = new Place(tojo.identifier())
                .make(this.home, MjAssemble.XMIR);
            new Saved(doc.toString(), target).value();
            tojo.withXmir(target);
        }
        return count;
    }

    private boolean spliced(final Xnav node) {
        boolean done = false;
        try {
            final Datum datum = new Constant(this.phino, node).value();
            new Xembler(
                Lowering.carrier(datum.forma(), datum.bytes())
            ).applyQuietly(node.node());
            done = true;
        } catch (final IllegalStateException | IOException ex) {
            Logger.debug(this, "A fragment stays unfolded: %s", ex.getMessage());
        }
        return done;
    }

    private static Directives carrier(final String forma, final String value) {
        final Directives dirs = new Directives();
        if ("number".equals(forma)) {
            if (value.length() != 23) {
                throw new IllegalStateException(
                    String.format("A number must dataize to eight bytes, not to '%s'", value)
                );
            }
            dirs.attr("base", "Φ.number")
                .xpath("node()").remove()
                .add("o").attr("as", "α0").attr("base", "Φ.bytes")
                .add("o").attr("as", "α0").set(value);
        } else if ("bool".equals(forma)) {
            if ("01-".equals(value)) {
                dirs.attr("base", "Φ.true");
            } else if ("00-".equals(value)) {
                dirs.attr("base", "Φ.false");
            } else {
                throw new IllegalStateException(
                    String.format("A bool must dataize to one byte, not to '%s'", value)
                );
            }
            dirs.xpath("node()").remove();
        } else {
            dirs.attr("base", "Φ.bytes")
                .xpath("node()").remove()
                .add("o").attr("as", "α0").set(value);
        }
        return dirs;
    }

    private static void selected(final Xnav node, final Collection<Xnav> out) {
        if (Lowering.foldable(node)) {
            out.add(node);
        } else {
            for (final Xnav kid : Lowering.kids(node)) {
                Lowering.selected(kid, out);
            }
        }
    }

    private static boolean foldable(final Xnav node) {
        final String base = Lowering.base(node);
        return base.length() > 1 && base.charAt(0) == '.'
            && Lowering.decided(node);
    }

    private static boolean decided(final Xnav node) {
        boolean good = Lowering.literal(node);
        if (!good) {
            final String base = Lowering.base(node);
            if (base.length() > 1 && base.charAt(0) == '.') {
                final List<Xnav> kids = Lowering.kids(node);
                good = !kids.isEmpty()
                    && kids.get(0).attribute("as").text().isEmpty();
                for (int idx = 1; good && idx < kids.size(); ++idx) {
                    good = kids.get(idx).attribute("as").text().isPresent();
                }
                for (int idx = 0; good && idx < kids.size(); ++idx) {
                    good = Lowering.decided(kids.get(idx));
                }
            }
        }
        return good;
    }

    private static boolean literal(final Xnav node) {
        final String base = Lowering.base(node);
        final List<Xnav> kids = Lowering.kids(node);
        final boolean good;
        if ("Φ.true".equals(base) || "Φ.false".equals(base)) {
            good = kids.isEmpty();
        } else if ("Φ.bytes".equals(base)) {
            good = kids.size() == 1
                && Lowering.base(kids.get(0)).isEmpty()
                && Lowering.kids(kids.get(0)).isEmpty();
        } else if ("Φ.number".equals(base) || "Φ.string".equals(base)) {
            good = kids.size() == 1 && Lowering.literal(kids.get(0));
        } else {
            good = false;
        }
        return good;
    }

    private static String base(final Xnav node) {
        return node.attribute("base").text().orElse("");
    }

    private static List<Xnav> kids(final Xnav node) {
        return node.elements(Filter.withName("o")).collect(Collectors.toList());
    }
}
