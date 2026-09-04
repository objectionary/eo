/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import org.eolang.lowering.Folded;
import org.eolang.lowering.Formas;
import org.eolang.lowering.Lowered;
import org.eolang.lowering.Outlined;
import org.eolang.lowering.Phino;
import org.eolang.lowering.Rewrite;

/**
 * Run the lowering passes of {@code eo-lowering} over every XMIR this
 * build compiles.
 *
 * <p>The engine lives in the {@code eo-lowering} module; this step only
 * feeds it. It reads the tables of {@link MjInference} once, builds the
 * passes — {@link Lowered} for the pure formations, {@link Outlined} for
 * the pure applications, {@link Folded} for the constant fragments — and
 * runs them in that order over each XMIR, in parallel across the files.
 * A file with nothing rewritten is neither saved nor repointed, so a
 * build without lowerable fragments leaves no trace of this step at
 * all.</p>
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
     * The directory with the tables of {@link MjInference}.
     */
    private final Path tables;

    /**
     * Ctor.
     * @param srcs XMIR sources to fold
     * @param target The directory for the folded XMIR
     * @param exe The binary that dataizes
     * @param types The directory with the tables of {@link MjInference}
     */
    Lowering(final Collection<TjForeign> srcs, final Path target,
        final Phino exe, final Path types) {
        this.sources = srcs;
        this.home = target;
        this.phino = exe;
        this.tables = types;
    }

    @Override
    public void exec() throws IOException {
        final Formas formas = new Formas(this.tables);
        final Path atoms = this.home.resolve(Lowering.ATOMS);
        final Iterable<Rewrite> passes = Arrays.asList(
            new Lowered(this.phino, formas, atoms),
            new Outlined(this.phino, formas, atoms),
            new Folded(this.phino)
        );
        Logger.info(
            this, "Folded or lowered %d fragment(s) in %d XMIR(s), into %[file]s",
            new Threaded<>(this.sources, tojo -> this.folded(tojo, passes)).total(),
            this.sources.size(), this.home
        );
    }

    private int folded(final TjForeign tojo, final Iterable<Rewrite> passes)
        throws IOException {
        final XMLDocument doc = new XMLDocument(tojo.xmir());
        final Xnav object = new Xnav(doc.inner());
        int count = 0;
        for (final Rewrite pass : passes) {
            count += pass.rewrite(object);
        }
        if (count > 0) {
            final Path target = new Place(tojo.identifier())
                .make(this.home, MjAssemble.XMIR);
            new Saved(doc.toString(), target).value();
            tojo.withXmir(target);
        }
        return count;
    }
}
