/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
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
 * file of its own, named after the number of the entry, into the directory
 * of the protocols. The entries have nothing to share: each of them brings
 * its own symbols, so a firing of one is never answered by the memo of
 * another, and an entry morphed alone fires exactly as many times as it
 * fires among the others. So the runs go side by side, one per processor,
 * and the build waits for its slowest entry rather than for the sum of all
 * of them.</p>
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
 * any one entry. Nothing is retried and nothing is skipped.</p>
 *
 * @since 0.74.0
 */
final class Morphing implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * The directory where the protocols are written, one per entry.
     */
    private final Path protocols;

    /**
     * The binary that morphs.
     */
    private final Phino phino;

    /**
     * The ceiling of nested morphing and dataization steps of one run.
     */
    private final int steps;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     * @param dest The directory where the protocols are written
     * @param exe The binary that morphs
     */
    Morphing(final Path dir, final Path dest, final Phino exe) {
        this(dir, dest, exe, 32);
    }

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     * @param dest The directory where the protocols are written
     * @param exe The binary that morphs
     * @param ceiling The ceiling of nested morphing and dataization steps
     */
    Morphing(final Path dir, final Path dest, final Phino exe, final int ceiling) {
        this.home = dir;
        this.protocols = dest;
        this.phino = exe;
        this.steps = ceiling;
    }

    @Override
    public void exec() throws IOException {
        final Path world = this.home.resolve("world.phi");
        if (!Files.exists(world)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while morphing needs the world the merging writes",
                    world
                )
            );
        }
        final Path entries = this.home.resolve("entries.tsv");
        if (!Files.exists(entries)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while morphing needs the entries the planting writes",
                    entries
                )
            );
        }
        final Path atoms = Files.write(
            this.home.resolve("atoms.yaml"),
            new UncheckedBytes(
                new BytesOf(new ResourceOf("org/eolang/lowering/atoms.yaml"))
            ).asBytes()
        );
        Files.createDirectories(this.protocols);
        final Collection<Integer> numbers = new ListOf<>(
            new Mapped<>(
                line -> Integer.parseInt(line.split("\t", -1)[0]),
                new Filtered<>(
                    line -> !line.isEmpty(),
                    new Mapped<>(Text::asString, new Split(new TextOf(entries), "\\R"))
                )
            )
        );
        final long start = System.currentTimeMillis();
        Logger.info(
            this,
            "Morphed %d entries of %[file]s in %[ms]s, up to %d nested steps each, into %[file]s",
            new IoChecked<>(
                new LengthOf(
                    new Threads<>(
                        Runtime.getRuntime().availableProcessors(),
                        new Mapped<Scalar<Path>>(
                            number -> () -> this.morph(world, atoms, number), numbers
                        )
                    )
                )
            ).value(),
            world,
            System.currentTimeMillis() - start,
            this.steps,
            this.protocols
        );
    }

    private Path morph(final Path world, final Path atoms, final int number)
        throws IOException {
        final long start = System.currentTimeMillis();
        final Path protocol = this.protocols.resolve(String.format("%d.xml", number));
        this.phino.morph(world, atoms, number, protocol, this.steps);
        Logger.debug(
            this,
            "Morphed the entry %d of %[file]s in %[ms]s into %[file]s",
            number, world, System.currentTimeMillis() - start, protocol
        );
        return protocol;
    }
}
