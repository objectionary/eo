/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.eolang.inference.Clues;
import org.eolang.inference.Demanded;
import org.eolang.inference.Depth;
import org.eolang.inference.Ladder;
import org.eolang.inference.Relayed;
import org.eolang.inference.Resolved;
import org.eolang.inference.Witnessed;
import org.eolang.parser.TrFull;

/**
 * Work out the types of the objects of a program and write down what is known.
 *
 * <p>The types of a whole program are worked out at once, because an object in
 * one file is used by objects in others and no single file can be typed on its
 * own. The XMIR of every file parsed so far is taken from the first directory,
 * prepared for the rules and left in the second, while the tables of what has
 * been worked out go to the third, a document each. {@link MjInference} names
 * all three.</p>
 *
 * <p>Every object keeps the type it was born with — nothing is ever renamed or
 * merged — and what we learn about those types goes into tables instead, one row
 * at a time. A type is named by the locator the parser gives every object,
 * {@code Φ.app.inc.φ.ρ}, which is unique across the whole program and needs no
 * dictionary to be read: a row saying that {@code Φ.app.t.next} has nothing
 * means something to a human as it stands. It also makes the tables of many
 * files one table, since no two files can name the same locator.</p>
 *
 * <p>Two things happen to a file before any rule looks at it. First, every
 * composite base is split into one object per dispatch step: the parser rolls
 * {@code x.next.foo} into a single base, while a rule can only notice an
 * attribute being taken when the taking is an object of its own. Second, the
 * locators are set again, because the objects that the splitting has just
 * created have none. That is what the prepared files hold, and they are worth
 * keeping: every row of every table points into them.</p>
 *
 * @since 0.67.0
 */
final class Inferring implements Step {

    /**
     * The directory with the XMIR files of the program.
     */
    private final Path input;

    /**
     * The directory for the prepared XMIR files, which keep the names they
     * had. It is emptied first, because the clues read it back as the whole
     * program: a file left there by an earlier run would put the types of a
     * deleted source into the tables.
     */
    private final Path prepared;

    /**
     * The directory for the tables, a document each.
     */
    private final Path tables;

    /**
     * Ctor.
     * @param parsed The directory with XMIR files, as the parser leaves them
     *  after its canonical pipeline (see {@code org.eolang.parser.Canonical})
     * @param pre The directory for the prepared XMIR files
     * @param rows The directory for the tables
     */
    Inferring(final Path parsed, final Path pre, final Path rows) {
        this.input = parsed;
        this.prepared = pre;
        this.tables = rows;
    }

    @Override
    public void exec() throws IOException {
        if (Files.exists(this.input)) {
            new Deleted(this.prepared.toFile()).get();
            final int ready = this.ready();
            final long start = System.currentTimeMillis();
            new Witnessed(new Relayed(new Demanded(new Resolved(new Clues()))))
                .follow(this.prepared, this.tables);
            Logger.info(
                this, "Inferred the types of %d XMIR(s) in %[ms]s",
                ready, System.currentTimeMillis() - start
            );
            Logger.info(
                this, "Inference tables are in %[file]s (%s)",
                this.tables, new Tabled(this.tables).asString()
            );
            this.measured();
        } else {
            Logger.info(
                this, "The directory %[file]s is absent, nothing to infer from it",
                this.input
            );
        }
    }

    private void measured() throws IOException {
        final Ladder ladder = new Depth(this.prepared, this.tables).ladder();
        Logger.info(
            this,
            "%d objects: %.1f%% named, %.1f%% rooted at a void, %.1f%% nothing known; depth %.1f%%",
            ladder.total(), ladder.named(), ladder.rooted(), ladder.blank(), ladder.percent()
        );
        for (final Map.Entry<String, Integer> rung : ladder.rungs().entrySet()) {
            Logger.debug(this, "  %6d  %s", rung.getValue(), rung.getKey());
        }
    }

    private int ready() throws IOException {
        final Xsline train = new Xsline(
            new TrFull(
                new TrDefault<>(
                    new StClasspath("/org/eolang/maven/inference/unroll-bases.xsl"),
                    new StClasspath("/org/eolang/parser/parse/set-locators.xsl")
                )
            )
        );
        int done = 0;
        for (final Path source : this.sources()) {
            final Path target = this.prepared.resolve(this.input.relativize(source));
            Files.createDirectories(target.getParent());
            Files.write(
                target,
                train.pass(new XMLDocument(source)).toString().getBytes(StandardCharsets.UTF_8)
            );
            done = done + 1;
        }
        return done;
    }

    private Collection<Path> sources() throws IOException {
        try (Stream<Path> found = Files.walk(this.input)) {
            return found
                .filter(path -> path.toString().endsWith(".xmir"))
                .filter(Files::isRegularFile)
                .sorted()
                .collect(Collectors.toList());
        }
    }
}
