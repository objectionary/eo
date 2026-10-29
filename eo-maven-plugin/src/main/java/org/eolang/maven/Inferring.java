/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.io.Directory;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Sorted;
import org.eolang.inference.Answers;
import org.eolang.inference.Facts;
import org.eolang.inference.Names;
import org.eolang.inference.Prepared;
import org.eolang.inference.Walk;

/**
 * Read what a program says about its own objects and write it down.
 *
 * <p>The facts of a whole program are read at once, because an object in
 * one file is used by objects in others and no single file can be read on
 * its own. The XMIR files are named by the caller, prepared and left in the
 * second directory, while {@code facts.xml} goes to the third.
 * {@link MjInference} names them from the registered tojos, and not from
 * whatever the parse directory still holds: a source that was deleted
 * leaves its XMIR behind, and reading that XMIR would put the facts of an
 * object nobody wrote any more into the document (#9271).</p>
 *
 * <p>Every object is named by the locator the parser gives it,
 * {@code Φ.app.inc.φ.ρ}, which is unique across the whole program and needs
 * no dictionary to be read. It also makes the facts of many files one
 * document, since no two files can name the same locator.</p>
 *
 * <p>Each file is made ready by {@link Prepared} before {@link Facts} reads
 * it: every chain of names is split into one object per name, and the
 * locators are set again on the objects the split made. That is what the
 * prepared files hold, and they are worth keeping, since every row of
 * {@code facts.xml} points into them.</p>
 *
 * <p>The engine then reads {@code facts.xml} alone and writes
 * {@code answers.xml} beside it: for every place, which objects it can be
 * when Java runs the program. It knows a few names of the program, the
 * {@link Names}, and nothing else of it.</p>
 *
 * @since 0.67.0
 */
final class Inferring implements Step {

    /**
     * The XMIR files the facts are read from.
     */
    private final Iterable<Path> sources;

    /**
     * The directory the XMIR files come from, which says where under
     * {@link #prepared} each of them lands.
     */
    private final Path input;

    /**
     * The directory for the prepared XMIR files, which keep the names they
     * had. It is emptied first, because {@link Facts} reads it back as the whole
     * program: a file left there by an earlier run would put the facts of a
     * deleted source into the document.
     */
    private final Path prepared;

    /**
     * The directory for {@code facts.xml} and {@code answers.xml}.
     */
    private final Path tables;

    /**
     * The names of the program the engine has to know.
     */
    private final Names names;

    /**
     * Ctor, for a caller that owns the directory and wants every XMIR in it.
     *
     * @param parsed The directory with XMIR files, as the parser leaves them
     *  after its canonical pipeline (see {@code org.eolang.parser.Canonical})
     * @param pre The directory for the prepared XMIR files
     * @param rows The directory for {@code facts.xml} and {@code answers.xml}
     */
    Inferring(final Path parsed, final Path pre, final Path rows) {
        this(new Directory(parsed), parsed, pre, rows, new Names());
    }

    /**
     * Ctor.
     *
     * @param xmirs The XMIR files the facts are read from
     * @param parsed The directory those files come from
     * @param pre The directory for the prepared XMIR files
     * @param rows The directory for {@code facts.xml} and {@code answers.xml}
     * @param known The names of the program the engine has to know
     */
    Inferring(
        final Iterable<Path> xmirs, final Path parsed, final Path pre, final Path rows,
        final Names known
    ) {
        this.sources = xmirs;
        this.input = parsed;
        this.prepared = pre;
        this.tables = rows;
        this.names = known;
    }

    @Override
    public void exec() throws IOException {
        if (Files.exists(this.input)) {
            new Deleted(this.prepared.toFile()).get();
            if (Files.exists(this.prepared)) {
                throw new IOException(
                    Logger.format(
                        "Can't clean up %[file]s: prepared XMIRs of an earlier run are still there, and inferring them together with the current ones would describe sources that no longer exist",
                        this.prepared
                    )
                );
            }
            Files.createDirectories(this.prepared);
            this.ready();
            new Facts().follow(this.prepared, this.tables);
            final long start = System.currentTimeMillis();
            new Answers(new Walk(this.tables.resolve("facts.xml"), this.names))
                .write(this.tables.resolve("answers.xml"));
            Logger.info(
                this,
                "The facts and the answers of the program are in %[file]s (%s), the answers took %[ms]s",
                this.tables, new Tabled(this.tables).asString(),
                System.currentTimeMillis() - start
            );
        } else {
            Logger.info(
                this, "The directory %[file]s is absent, nothing to infer from it",
                this.input
            );
        }
    }

    private void ready() throws IOException {
        final Prepared train = new Prepared();
        for (final Path source : new Sorted<>(
            new Filtered<>(
                path -> path.toString().endsWith(".xmir") && Files.isRegularFile(path),
                this.sources
            )
        )) {
            final Path target = this.prepared.resolve(this.input.relativize(source));
            Files.createDirectories(target.getParent());
            Files.write(
                target,
                train.apply(new XMLDocument(source)).toString().getBytes(StandardCharsets.UTF_8)
            );
        }
    }
}
