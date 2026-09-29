/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Collection;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeoutException;
import org.cactoos.Proc;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.UncheckedBytes;
import org.cactoos.experimental.Threads;
import org.cactoos.io.ResourceOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;

/**
 * The morphing of every entry of the world, one run of phino per entry.
 *
 * <p>Every formation of the build is folded by a call of the binary aimed
 * at the mark of its entry, and the protocol of that call is written as a
 * file of its own into {@code 7-lowering-protocols}, at the path the
 * locator of the formation names, so the protocol of
 * {@code Φ.bytes.as-hex} is {@code bytes/as-hex.xml}. The entries have
 * nothing to share: each of them brings its own symbols, so a firing of one is never
 * answered by the memo of another, and an entry morphed alone fires
 * exactly as many times as it fires among the others. So the runs go side
 * by side, one per processor, and the build waits for its slowest entry
 * rather than for the sum of all of them. While they go, a line every
 * thirty seconds says how many entries are morphed, how long it has taken,
 * and how many bytes of protocols are written, since one slow entry may
 * keep the build silent for minutes.</p>
 *
 * <p>What the binary can say about a primitive is said in
 * {@code atoms.yaml} and nowhere else, and that table is written beside
 * the world from the resource of the same name before the runs, so that
 * what phino was told stays next to what it answered. A lambda no entry
 * of that file matches is left standing where it is, and the formation
 * that reached it is a taint: nothing is guessed about it, and nothing is
 * folded.</p>
 *
 * <p>Every run is bounded by a ceiling of nested steps, because a
 * formation that grows on every round, a loop counting up for one, never
 * comes back to a term it has seen, so no guard against cycles can stop
 * it. At the ceiling the binary leaves that formation standing as a taint,
 * and the build fails only when the binary itself exits with an error, on
 * any one entry. Every run is also bounded by a budget of time, since the
 * ceiling bounds the depth of a run and not its width: a run still going
 * when its budget is spent is killed, its protocol is deleted, and the
 * formation stays as it was written, the way a taint does. Nothing is
 * retried.</p>
 *
 * @since 0.74.0
 */
final class Morphing implements Proc<Path> {

    /**
     * The binary that morphs.
     */
    private final Phino phino;

    /**
     * The ceiling of nested morphing and dataization steps of one run.
     */
    private final int steps;

    /**
     * The time one run may take before it is killed.
     */
    private final Duration budget;

    /**
     * Ctor.
     *
     * @param exe The binary that morphs
     */
    Morphing(final Phino exe) {
        this(exe, 32);
    }

    /**
     * Ctor.
     *
     * @param exe The binary that morphs
     * @param ceiling The ceiling of nested morphing and dataization steps
     */
    Morphing(final Phino exe, final int ceiling) {
        this(exe, ceiling, Duration.ofSeconds(60L));
    }

    /**
     * Ctor.
     *
     * @param exe The binary that morphs
     * @param ceiling The ceiling of nested morphing and dataization steps
     * @param span The time one run may take before it is killed
     */
    Morphing(final Phino exe, final int ceiling, final Duration span) {
        this.phino = exe;
        this.steps = ceiling;
        this.budget = span;
    }

    @Override
    public void exec(final Path target) throws IOException {
        final Path home = target.resolve("7-lowering");
        final Path world = home.resolve("world.phi");
        if (!Files.exists(world)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while morphing needs the world the merging writes",
                    world
                )
            );
        }
        final Path entries = home.resolve("entries.tsv");
        if (!Files.exists(entries)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while morphing needs the entries the planting writes",
                    entries
                )
            );
        }
        final Path atoms = Files.write(
            home.resolve("atoms.yaml"),
            new UncheckedBytes(
                new BytesOf(new ResourceOf("org/eolang/lowering/atoms.yaml"))
            ).asBytes()
        );
        final Path protocols = Files.createDirectories(
            target.resolve("7-lowering-protocols")
        );
        final Collection<String> rows = new ListOf<>(
            new Filtered<>(
                line -> !line.isEmpty(),
                new Mapped<>(Text::asString, new Split(new TextOf(entries), "\\R"))
            )
        );
        final long start = System.currentTimeMillis();
        final Progress progress = new Progress(rows.size());
        final Timer ticker = new Timer("morphing-progress", true);
        ticker.scheduleAtFixedRate(
            new TimerTask() {
                @Override
                public void run() {
                    Logger.info(Morphing.this, "Morphed %s so far", progress.asString());
                }
            },
            30_000L,
            30_000L
        );
        try {
            Logger.info(
                this,
                "Morphed %d entries of %[file]s in %[ms]s, up to %d nested steps each, into %[file]s",
                new IoChecked<>(
                    new LengthOf(
                        new Threads<>(
                            Runtime.getRuntime().availableProcessors(),
                            new Mapped<Scalar<Path>>(
                                row -> () -> this.morph(
                                    world, atoms, protocols, row, progress
                                ),
                                rows
                            )
                        )
                    )
                ).value(),
                world,
                System.currentTimeMillis() - start,
                this.steps,
                protocols
            );
        } finally {
            ticker.cancel();
        }
    }

    private Path morph(
        final Path world, final Path atoms, final Path protocols, final String row,
        final Progress progress
    ) throws IOException {
        final long start = System.currentTimeMillis();
        final String[] cells = row.split("\t", -1);
        final int number = Integer.parseInt(cells[0]);
        if (!cells[1].startsWith("Φ.")) {
            throw new IllegalStateException(
                String.format(
                    "The locator '%s' of the entry %d does not start with 'Φ.', while its protocol is named after the path below it",
                    cells[1], number
                )
            );
        }
        final Path protocol = protocols.resolve(
            String.format("%s.xml", cells[1].substring(2).replace('.', '/'))
        );
        Files.createDirectories(protocol.getParent());
        try {
            this.phino.morph(world, atoms, number, protocol, this.steps, this.budget);
            Logger.debug(
                this,
                "Morphed the entry %d of %[file]s in %[ms]s into %[file]s",
                number, world, System.currentTimeMillis() - start, protocol
            );
            progress.add(protocol);
        } catch (final TimeoutException ex) {
            Files.deleteIfExists(protocol);
            Logger.warn(
                this,
                "The entry %d at %s was killed after %[ms]s, so it has no protocol",
                number, cells[1], this.budget.toMillis()
            );
        }
        return protocol;
    }
}
