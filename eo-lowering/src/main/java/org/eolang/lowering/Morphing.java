/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import io.github.sekator778.seqdir.Seqdir;
import io.github.sekator778.seqdir.Sequence;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.Collection;
import java.util.Comparator;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Stream;
import org.cactoos.Proc;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.experimental.Threads;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
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
 * in {@code 2-protocols}, inside the home directory of the lowering, at a
 * path made from the locator of the object, so the protocol of
 * {@code Φ.bytes.as-hex} is {@code bytes/as-hex.xml}.</p>
 *
 * <p>The entries do not depend on each other. Every entry has its own
 * symbols, so one entry never reuses a result of another, and one entry
 * takes the same number of steps alone as it takes together with the
 * others. This is why many runs of phino work at the same time, one for
 * every processor. The build waits only as long as its slowest entry, and
 * not as long as all the entries together. One slow entry can keep the
 * build silent for minutes, so every thirty seconds this stage prints a
 * line from {@link Progress} that says how much work is done, and which
 * entries phino is still working on.</p>
 *
 * <p>phino is allowed to write down only the operations that are listed in
 * the file {@code atoms.yaml}, such as adding two numbers. {@link Planting}
 * writes that file into the directory of the world, next to where phino
 * answers: the operations of the atoms of the library, from the resources
 * of the module, and one row for every object the planting gave a row
 * to. When phino meets an atom that is not in the file, it leaves that
 * atom as it is. Then the entry becomes a taint: nothing is guessed about
 * it, and nothing about it is turned into Java.</p>
 *
 * <p>Every run has two limits. The first limit is the largest number of
 * steps phino may take inside one another. This limit is needed because
 * some objects grow on every round, like a loop that counts up. Such an
 * object never repeats itself, so phino cannot see that it is going around
 * in a circle. When phino reaches this limit, it stops working on that
 * object, and the entry becomes a taint. The second limit is time. The
 * first limit controls how deep phino goes, but not how wide, so a run may
 * still take a very long time. When a run is still working after its time
 * is over, phino stops by itself, and the object stays in EO, the same way
 * as a taint does. Its protocol stays on the disk for a reader to study,
 * with a {@code timeout} element where the time ran out, but it is not
 * kept in the cache, and {@link Rendering} makes no atom from it. The next
 * build tries this run again. The build fails only when phino itself
 * fails with an error on some entry. Nothing is tried twice in one
 * build.</p>
 *
 * <p>phino runs only on the entries that the {@link Scope} given to the
 * constructor covers, which helps to study one slow entry alone, or to keep
 * one entry away from phino. The other entries get no protocol, and they
 * stay in EO.</p>
 *
 * <p>A protocol stays the same as long as six things stay the same: the
 * part of the world that its entry uses, as {@link Uses} says, the table of
 * operations, the version of phino, its limit of memory, the limit of
 * steps, and the limit of loops. So, every protocol is saved in the cache,
 * together with these six things. When a later build has the same six
 * things, it takes the protocol from the cache and does not run phino at
 * all. An object that changed makes phino run again only on the entries
 * that use it. A run that was stopped saves nothing in the cache, so the
 * next build tries it again. The protocols of an earlier build are deleted
 * before the runs, so that an entry that is not in the world any more
 * leaves no protocol behind.</p>
 *
 * <p>When the constructor asks for it, this stage also keeps the protocol
 * of every entry as indented text, which a person can read without any
 * tool. The same run of phino that writes the XML protocol of an entry
 * writes its text too, into a file whose name ends with {@code .txt}, so
 * no entry is computed twice. Only when the cache answers the protocol
 * and has no text for it does phino run again, for the text alone. These texts land in
 * a numbered {@code texts} directory beside {@code 2-protocols}, so the
 * text of {@code Φ.bytes.as-hex} is {@code texts/bytes/as-hex.txt}. Nothing in
 * the build reads them. The cache keeps them the same way as the
 * protocols, so a later build with the same four things does not run
 * phino for them either. The text of a run that was stopped stays on the
 * disk for study, with a {@code timeout …} line where the time ran out.
 * The texts of an earlier build are deleted before the runs, even when
 * this build does not ask for them, so that no text outlives its
 * protocol.</p>
 *
 * @since 0.64.0
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
     * The entries that phino is allowed to run on.
     */
    private final Scope scope;

    /**
     * The largest number of steps inside one another that one run may take.
     */
    private final int steps;

    /**
     * How many times one run may enter the same formation again.
     */
    private final int loops;

    /**
     * The time that one run may take before it is stopped.
     */
    private final Duration budget;

    /**
     * Whether every entry gets its protocol as indented text too.
     */
    private final boolean text;

    /**
     * Ctor.
     *
     * @param exe The phino program, which does the morphing
     * @param store The cache, where the protocols are kept between builds
     * @param range The entries that phino is allowed to run on
     * @param ceiling The largest number of steps inside one another
     * @param again How many times one run may enter the same formation again
     * @param span The time that one run may take before it is stopped
     * @param texts Whether every entry gets its protocol as indented text too
     */
    Morphing(
        final Phino exe, final GlobalCache store, final Scope range, final int ceiling,
        final int again, final Duration span, final boolean texts
    ) {
        this.phino = exe;
        this.cache = store;
        this.scope = range;
        this.steps = ceiling;
        this.loops = again;
        this.budget = span;
        this.text = texts;
    }

    @Override
    public void exec(final Path home) throws IOException {
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
        final Path atoms = home.resolve("atoms.yaml");
        if (!Files.exists(atoms)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while morphing needs the table of operations the planting writes",
                    atoms
                )
            );
        }
        final Sequence dirs = new Seqdir(home, 1).dirs();
        final Path legacy = home.resolve("texts");
        final Path numbered = dirs.find("texts").orElse(legacy);
        final Path protocols = dirs.once("protocols");
        for (final Path dir : new ListOf<>(protocols, legacy, numbered)) {
            Morphing.clean(dir);
        }
        Files.deleteIfExists(legacy);
        Files.createDirectories(protocols);
        final GlobalCache store = this.phino.narrowed(this.cache)
            .with(new UncheckedText(new HexOf(new Sha256DigestOf(new InputOf(atoms)))).asString())
            .with(String.valueOf(this.steps))
            .with(String.valueOf(this.loops));
        final Uses uses = new Uses(home);
        final Collection<String> rows = new ListOf<>(
            new Filtered<>(
                line -> !line.isEmpty()
                    && this.scope.covers(line.split("\t", -1)[1]),
                new Mapped<>(Text::asString, new Split(new TextOf(entries), "\\R"))
            )
        );
        final Path texts;
        if (this.text && !rows.isEmpty()) {
            texts = dirs.once("texts");
        } else {
            texts = numbered;
        }
        final Progress progress = new Progress(rows.size());
        final Timer ticker = new Timer("morphing-progress", true);
        ticker.scheduleAtFixedRate(
            new TimerTask() {
                @Override
                public void run() {
                    Logger.info(Morphing.this, "Morphed %s", progress.asString());
                }
            },
            30_000L,
            30_000L
        );
        try {
            Logger.info(
                this,
                "Ran %d entries of %[file]s covered by %s, up to %d nested steps each, into %[file]s: %s",
                new IoChecked<>(
                    new LengthOf(
                        new Threads<>(
                            Runtime.getRuntime().availableProcessors(),
                            new Mapped<Scalar<Path>>(
                                row -> () -> this.morph(
                                    world, atoms, protocols, texts, row, progress, store, uses
                                ),
                                rows
                            )
                        )
                    )
                ).value(),
                world,
                this.scope,
                this.steps,
                protocols,
                progress.asString()
            );
        } finally {
            ticker.cancel();
        }
    }

    private static void clean(final Path dir) throws IOException {
        if (Files.exists(dir)) {
            try (Stream<Path> inside = Files.walk(dir)) {
                for (final Path stale : new ListOf<>(
                    inside.filter(path -> !path.equals(dir))
                        .sorted(Comparator.reverseOrder())
                        .iterator()
                )) {
                    Files.delete(stale);
                }
            }
        }
    }

    private Path morph(
        final Path world, final Path atoms, final Path protocols, final Path texts,
        final String row, final Progress progress, final GlobalCache store, final Uses uses
    ) throws IOException {
        final long start = System.currentTimeMillis();
        final String[] cells = row.split("\t", -1);
        final int number = Integer.parseInt(cells[0]);
        final Locator locator = new Locator(cells[1]);
        final Path tail = locator.protocol();
        final Path protocol = protocols.resolve(tail);
        final Path minutes = texts.resolve(locator.text());
        final Path draft = minutes.resolveSibling(
            String.format("%s.draft", minutes.getFileName())
        );
        Files.createDirectories(protocol.getParent());
        if (this.text) {
            Files.createDirectories(minutes.getParent());
        }
        final String hash = uses.hash(number, cells[1]);
        final AtomicBoolean fresh = new AtomicBoolean();
        progress.begin(cells[1]);
        try {
            store.kept(
                tail,
                () -> hash,
                (src, tgt) -> false,
                (src, tgt) -> {
                    fresh.set(true);
                    if (this.text) {
                        this.phino.dataize(
                            src, atoms, number, tgt, draft, this.steps, this.loops, this.budget
                        );
                    } else {
                        this.phino.dataize(
                            src, atoms, number, tgt, this.steps, this.loops, this.budget
                        );
                    }
                    return tgt;
                }
            ).apply(world, protocol);
            if (this.text) {
                store.kept(
                    locator.text(),
                    () -> hash,
                    (src, tgt) -> false,
                    (src, tgt) -> {
                        if (fresh.get()) {
                            Files.move(draft, tgt, StandardCopyOption.REPLACE_EXISTING);
                        } else {
                            this.phino.transcribe(
                                src, atoms, number, tgt, this.steps, this.loops, this.budget
                            );
                        }
                        return tgt;
                    }
                ).apply(world, minutes);
            }
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
            if (Files.exists(draft)) {
                Files.move(draft, minutes, StandardCopyOption.REPLACE_EXISTING);
            }
            Logger.warn(
                this,
                "Lowering of %s ran out of time budget (%[ms]s), its protocol kept in %[file]s for study",
                cells[1], this.budget.toMillis(), protocol
            );
            progress.expire();
        } finally {
            progress.end(cells[1]);
        }
        return protocol;
    }
}
