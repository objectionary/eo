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
 * A report of how much of the work of {@link Morphing} is done.
 *
 * <p>For eo-runtime, running phino on all the entries takes many minutes,
 * and one entry alone may take most of that time. So, while the work is
 * still going on, {@link Morphing} prints this report from time to time.
 * The report says how many entries are done, how much time has passed,
 * how many results came from the cache, and how big the protocols are.</p>
 *
 * <p>Many runs of phino work at the same time, in different threads, and
 * any of them may finish at any moment. This is why every counter here is
 * safe to change from many threads at once.</p>
 *
 * @since 0.74.0
 */
final class Progress implements Text {

    /**
     * How many entries there are in total.
     */
    private final int total;

    /**
     * The moment the work started, in milliseconds.
     */
    private final long start;

    /**
     * How many entries are done so far.
     */
    private final AtomicInteger done;

    /**
     * How many of the entries done so far got their protocols from the cache.
     */
    private final AtomicInteger reused;

    /**
     * How many bytes all the protocols written so far take.
     */
    private final AtomicLong bytes;

    /**
     * Ctor.
     *
     * @param entries How many entries there are in total
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
     * @param entries How many entries there are in total
     * @param moment The moment the work started, in milliseconds
     * @param count How many entries are done so far
     * @param hits How many of them got their protocols from the cache
     * @param size How many bytes all the protocols written so far take
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
     * Count one more entry as done, whose protocol phino has just written.
     *
     * @param protocol The protocol file of the entry
     * @throws IOException If the size of the protocol file cannot be read
     */
    void add(final Path protocol) throws IOException {
        this.bytes.addAndGet(Files.size(protocol));
        this.done.incrementAndGet();
    }

    /**
     * Count one more entry as done, whose protocol came from the cache.
     *
     * @param protocol The protocol file of the entry
     * @throws IOException If the size of the protocol file cannot be read
     */
    void reuse(final Path protocol) throws IOException {
        this.add(protocol);
        this.reused.incrementAndGet();
    }
}
