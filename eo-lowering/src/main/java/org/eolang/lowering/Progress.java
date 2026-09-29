/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import org.cactoos.Text;

/**
 * How far the morphing of the world has come.
 *
 * <p>A world of the eo-runtime takes its entries many minutes to morph,
 * and a single entry may run for most of them, so the morphing says how
 * far it has come while it is still on the way. The runs go side by side,
 * which is why every count here is one any of them may add to at any
 * moment.</p>
 *
 * @since 0.74.0
 */
final class Progress implements Text {

    /**
     * How many entries there are to morph.
     */
    private final int total;

    /**
     * The moment the morphing started, in milliseconds.
     */
    private final long start;

    /**
     * How many entries are morphed so far.
     */
    private final AtomicInteger done;

    /**
     * How many of the entries morphed so far took their protocols from the cache.
     */
    private final AtomicInteger reused;

    /**
     * How many bytes of protocols are written so far.
     */
    private final AtomicLong bytes;

    /**
     * Ctor.
     *
     * @param entries How many entries there are to morph
     */
    Progress(final int entries) {
        this(
            entries,
            System.currentTimeMillis(),
            new AtomicInteger(),
            new AtomicInteger(),
            new AtomicLong()
        );
    }

    /**
     * Ctor.
     *
     * @param entries How many entries there are to morph
     * @param moment The moment the morphing started, in milliseconds
     * @param count How many entries are morphed so far
     * @param hits How many of them took their protocols from the cache
     * @param size How many bytes of protocols are written so far
     */
    Progress(
        final int entries, final long moment, final AtomicInteger count,
        final AtomicInteger hits, final AtomicLong size
    ) {
        this.total = entries;
        this.start = moment;
        this.done = count;
        this.reused = hits;
        this.bytes = size;
    }

    @Override
    public String asString() {
        return Logger.format(
            "%d of %d entries in %[ms]s, %d of them from cache, %[size]s of protocols",
            this.done.get(),
            this.total,
            System.currentTimeMillis() - this.start,
            this.reused.get(),
            this.bytes.get()
        );
    }

    /**
     * Count one more entry morphed, with the protocol it was morphed into.
     *
     * @param protocol The protocol of the entry
     * @throws IOException If the protocol cannot be read
     */
    void add(final Path protocol) throws IOException {
        this.bytes.addAndGet(Files.size(protocol));
        this.done.incrementAndGet();
    }

    /**
     * Count one more entry morphed, with the protocol it took from the cache.
     *
     * @param protocol The protocol of the entry
     * @throws IOException If the protocol cannot be read
     */
    void reuse(final Path protocol) throws IOException {
        this.add(protocol);
        this.reused.incrementAndGet();
    }
}
