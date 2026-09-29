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
import java.util.Comparator;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicBoolean;
import org.cactoos.Proc;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.bytes.UncheckedBytes;
import org.cactoos.experimental.Threads;
import org.cactoos.io.Directory;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.iterable.Sorted;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;
import org.cactoos.text.HexOf;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.cache.GlobalCache;

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
 * retried within a build.</p>
 *
 * <p>A protocol is the same for as long as the world, the table of atoms,
 * the version of phino and the ceiling are the same, so every protocol is
 * kept in the cache under all four of them, and a build that brings them
 * again takes it from there instead of running the binary. A killed run
 * leaves nothing in the cache, and is tried again by the next build. The
 * protocols of an earlier build are deleted before the runs, so that an
 * entry the world no longer has leaves no protocol behind.</p>
 *
 * @since 0.74.0
 * @todo #8548:90min Key every protocol in the cache by the part of the
 *  world its entry reaches, not by the whole world. Now a change of any
 *  one formation of the build changes the hash of {@code world.phi}, and
 *  every entry is morphed again, even those that never reach the changed
 *  formation. The key could be made from the formation of the entry and
 *  the formations it refers to, walked to the end.
 */
final class Morphing implements Proc<Path> {

    /**
     * The binary that morphs.
     */
    private final Phino phino;

    /**
     * The cache the protocols are kept in between builds.
     */
    private final GlobalCache cache;

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
     * @param store The cache the protocols are kept in between builds
     */
    Morphing(final Phino exe, final GlobalCache store) {
        this(exe, store, 32, Duration.ofSeconds(60L));
    }

    /**
     * Ctor.
     *
     * @param exe The binary that morphs
     * @param store The cache the protocols are kept in between builds
     * @param ceiling The ceiling of nested morphing and dataization steps
     * @param span The time one run may take before it is killed
     */
    Morphing(
        final Phino exe, final GlobalCache store, final int ceiling, final Duration span
    ) {
        this.phino = exe;
        this.cache = store;
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
        final Path protocols = target.resolve("7-lowering-protocols");
        if (Files.exists(protocols)) {
            for (final Path stale
                : new Sorted<>(Comparator.reverseOrder(), new Directory(protocols))) {
                Files.delete(stale);
            }
        }
        Files.createDirectories(protocols);
        final GlobalCache store = this.cache
            .with(this.phino.pin())
            .with(new UncheckedText(new HexOf(new Sha256DigestOf(new InputOf(atoms)))).asString())
            .with(String.valueOf(this.steps));
        final String hash = new UncheckedText(
            new HexOf(new Sha256DigestOf(new InputOf(world)))
        ).asString();
        final Collection<String> rows = new ListOf<>(
            new Filtered<>(
                line -> !line.isEmpty(),
                new Mapped<>(Text::asString, new Split(new TextOf(entries), "\\R"))
            )
        );
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
                "Ran %d entries of %[file]s, up to %d nested steps each, into %[file]s: %s",
                new IoChecked<>(
                    new LengthOf(
                        new Threads<>(
                            Runtime.getRuntime().availableProcessors(),
                            new Mapped<Scalar<Path>>(
                                row -> () -> this.morph(
                                    world, atoms, protocols, row, progress, store, hash
                                ),
                                rows
                            )
                        )
                    )
                ).value(),
                world,
                this.steps,
                protocols,
                progress.asString()
            );
        } finally {
            ticker.cancel();
        }
    }

    private Path morph(
        final Path world, final Path atoms, final Path protocols, final String row,
        final Progress progress, final GlobalCache store, final String hash
    ) throws IOException {
        final long start = System.currentTimeMillis();
        final String[] cells = row.split("\t", -1);
        final int number = Integer.parseInt(cells[0]);
        final Path tail = new Locator(cells[1]).protocol();
        final Path protocol = protocols.resolve(tail);
        Files.createDirectories(protocol.getParent());
        final AtomicBoolean fresh = new AtomicBoolean();
        try {
            store.kept(
                tail,
                () -> hash,
                (src, tgt) -> false,
                (src, tgt) -> {
                    fresh.set(true);
                    this.phino.morph(src, atoms, number, tgt, this.steps, this.budget);
                    return tgt;
                }
            ).apply(world, protocol);
            if (fresh.get()) {
                Logger.debug(
                    this,
                    "Morphed the entry %d of %[file]s in %[ms]s into %[file]s",
                    number, world, System.currentTimeMillis() - start, protocol
                );
                progress.add(protocol);
            } else {
                Logger.debug(
                    this,
                    "Took the protocol of the entry %d of %[file]s from cache into %[file]s",
                    number, world, protocol
                );
                progress.reuse(protocol);
            }
        } catch (final KilledException ex) {
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
