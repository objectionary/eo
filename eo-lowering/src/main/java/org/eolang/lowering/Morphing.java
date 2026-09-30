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
 * The stage that runs phino on every entry, one run for every entry.
 *
 * <p>For every entry, this stage asks phino to compute the body of the
 * entry with symbols as its inputs. This is called "morphing". phino
 * writes down every step it takes into a protocol file. The file is saved
 * in {@code 7-lowering-protocols}, at a path made from the locator of the
 * object, so the protocol of {@code Φ.bytes.as-hex} is
 * {@code bytes/as-hex.xml}.</p>
 *
 * <p>The entries do not depend on each other. Every entry has its own
 * symbols, so one entry never reuses a result of another, and one entry
 * takes the same number of steps alone as it takes together with the
 * others. This is why many runs of phino work at the same time, one for
 * every processor. The build waits only as long as its slowest entry, and
 * not as long as all the entries together. One slow entry can keep the
 * build silent for minutes, so every thirty seconds this stage prints a
 * line from {@link Progress} that says how much work is done.</p>
 *
 * <p>phino is allowed to write down only the operations that are listed in
 * the file {@code atoms.yaml}, such as adding two numbers. Before the runs,
 * this stage copies that file from the resources of the module into the
 * directory of the world, so that anybody can see what phino was told,
 * next to what phino answered. When phino meets an atom that is not in the
 * file, it leaves that atom as it is. Then the entry becomes a taint:
 * nothing is guessed about it, and nothing about it is turned into
 * Java.</p>
 *
 * <p>Every run has two limits. The first limit is the largest number of
 * steps phino may take inside one another. This limit is needed because
 * some objects grow on every round, like a loop that counts up. Such an
 * object never repeats itself, so phino cannot see that it is going around
 * in a circle. When phino reaches this limit, it stops working on that
 * object, and the entry becomes a taint. The second limit is time. The
 * first limit controls how deep phino goes, but not how wide, so a run may
 * still take a very long time. When a run is still working after its time
 * is over, it is stopped, its protocol is deleted, and the object stays in
 * EO, the same way as a taint does. The build fails only when phino itself
 * fails with an error on some entry. Nothing is tried twice in one
 * build.</p>
 *
 * <p>A protocol stays the same as long as four things stay the same: the
 * world, the table of operations, the version of phino, and the limit of
 * steps. So, every protocol is saved in the cache, together with these
 * four things. When a later build has the same four things, it takes the
 * protocol from the cache and does not run phino at all. A run that was
 * stopped saves nothing in the cache, so the next build tries it again.
 * The protocols of an earlier build are deleted before the runs, so that
 * an entry that is not in the world any more leaves no protocol
 * behind.</p>
 *
 * @since 0.74.0
 * @todo #8548:90min Save every protocol in the cache under the part of the
 *  world that its entry uses, and not under the whole world. Now, when
 *  any one object of the build changes, the hash of {@code world.phi}
 *  changes too, and phino runs again on every entry, even on the entries
 *  that never use the changed object. The key could be made from the
 *  object of the entry and all the objects it uses, directly or through
 *  other objects.
 */
final class Morphing implements Proc<Path> {

    /**
     * The phino program, which does the morphing.
     */
    private final Phino phino;

    /**
     * The cache, where the protocols are kept from one build to the next.
     */
    private final GlobalCache cache;

    /**
     * The largest number of steps inside one another that one run may take.
     */
    private final int steps;

    /**
     * The time that one run may take before it is stopped.
     */
    private final Duration budget;

    /**
     * Ctor.
     *
     * @param exe The phino program, which does the morphing
     * @param store The cache, where the protocols are kept between builds
     * @param span The time that one run may take before it is stopped
     */
    Morphing(final Phino exe, final GlobalCache store, final Duration span) {
        this(exe, store, 32, span);
    }

    /**
     * Ctor.
     *
     * @param exe The phino program, which does the morphing
     * @param store The cache, where the protocols are kept between builds
     * @param ceiling The largest number of steps inside one another
     * @param span The time that one run may take before it is stopped
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
