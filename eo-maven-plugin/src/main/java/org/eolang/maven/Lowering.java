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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.eolang.lowering.Boxed;
import org.eolang.lowering.Boxes;
import org.eolang.lowering.Formas;
import org.eolang.lowering.Home;
import org.eolang.lowering.Lowered;
import org.eolang.lowering.Phino;
import org.eolang.lowering.Planted;
import org.eolang.lowering.Xml;

/**
 * Lower every fragment of every XMIR this build compiles, through the
 * engine of {@code eo-lowering}.
 *
 * <p>The step feeds the engine and decides nothing itself. It reads the
 * tables of {@link MjInference} once, plants a box on every formation of
 * every document that declares arguments, writes the boxed variant of
 * each document for the runs of the other documents to merge into their
 * world, and then lowers the documents in parallel: {@link Lowered}
 * rewrites each of them in place, one run of phino per fragment. A
 * document with nothing rewritten is neither saved nor repointed, so a
 * build without lowerable fragments leaves only the boxes behind.</p>
 *
 * @since 0.76.0
 * @todo #8548:60min The boxed variants of a build pile up under the
 *  {@code boxed} directory across builds, so a document deleted from the
 *  sources since the last build still joins the world of every run and may
 *  clash with the object that replaced it. Let's clean the directory before
 *  writing the variants, or write them under a directory named after the
 *  build, so a run merges the documents of this build alone.
 */
final class Lowering implements Step {

    /**
     * The directory for the lowered XMIR.
     */
    static final String DIR = "4-lower";

    /**
     * The file that says lowering ran, and with what.
     */
    static final String MARKER = "lowering.txt";

    /**
     * XMIR sources to lower.
     */
    private final Collection<TjForeign> sources;

    /**
     * The directory to write the lowered XMIR to.
     */
    private final Path home;

    /**
     * The binary that morphs.
     */
    private final Phino phino;

    /**
     * The directory with the tables of {@link MjInference}.
     */
    private final Path tables;

    /**
     * Ctor.
     *
     * @param srcs XMIR sources to lower
     * @param target The directory for the lowered XMIR
     * @param exe The binary that morphs
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
        final Home dir = new Home(this.home);
        final Boxes boxes = new Boxes(dir.boxes());
        final List<Path> docs = new ArrayList<>(this.sources.size());
        for (final TjForeign tojo : this.sources) {
            docs.add(tojo.xmir());
        }
        boxes.save(new Planted(docs, formas).all());
        Logger.debug(
            this, "Boxed %d XMIR(s) into %[file]s",
            new Threaded<>(this.sources, tojo -> Lowering.boxed(tojo, dir, boxes)).total(),
            dir.boxes()
        );
        Logger.info(
            this, "Lowered %d fragment(s) in %d XMIR(s), into %[file]s",
            new Threaded<>(this.sources, tojo -> this.lowered(tojo, formas, dir)).total(),
            this.sources.size(), this.home
        );
    }

    private static int boxed(final TjForeign tojo, final Home dir, final Boxes boxes)
        throws IOException {
        new Xml(
            new Boxed(new XMLDocument(tojo.xmir()).inner(), boxes, "").copy()
        ).saved(dir.boxed(tojo.identifier()));
        return 1;
    }

    private int lowered(final TjForeign tojo, final Formas formas, final Home dir)
        throws IOException {
        final XMLDocument doc = new XMLDocument(tojo.xmir());
        final int count = new Lowered(this.phino, formas, dir, tojo.identifier())
            .rewrite(new Xnav(doc.inner()));
        if (count > 0) {
            final Path target = new Place(tojo.identifier())
                .make(this.home, MjAssemble.XMIR);
            new Saved(doc.toString(), target).value();
            tojo.withXmir(target);
        }
        return count;
    }
}
