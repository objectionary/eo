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
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import org.cactoos.Text;
import org.cactoos.iterable.HeadOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;

/**
 * A report of how much of the work of {@link Morphing} is done.
 *
 * <p>For eo-runtime, running phino on all the entries takes many minutes,
 * and one entry alone may take most of that time. So, while the work is
 * still going on, {@link Morphing} prints this report from time to time.
 * The report says how many entries are done, how much time has passed,
 * how many results came from the cache, and how big the protocols are.
 * At the end, it names the entries that phino is still working on, the
 * oldest first, so a reader sees which ones keep the build waiting. Only
 * the first five are named, and the rest are only counted, as in
 * {@code +12}. Every name is written without the {@code Φ.} at its
 * start, so {@code Φ.number.exp} is written as {@code number.exp}.</p>
 *
 * <p>Many runs of phino work at the same time, in different threads, and
 * any of them may finish at any moment. This is why every counter here is
 * safe to change from many threads at once.</p>
 *
 * @since 0.64.0
 */
final class Progress implements Text {

    /**
     * How many entries in processing the report names, at most.
     */
    private static final int SHOWN = 5;

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
     * The locators of the entries in processing, the oldest first.
     */
    private final Collection<String> busy;

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
            new AtomicLong(),
            new ConcurrentLinkedQueue<>()
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
     * @param running The locators of the entries in processing, the oldest first
     */
    Progress(
        final int entries, final long moment, final AtomicInteger count,
        final AtomicInteger hits, final AtomicLong size, final Collection<String> running
    ) {
        this.total = entries;
        this.start = moment;
        this.done = count;
        this.reused = hits;
        this.bytes = size;
        this.busy = running;
    }

    @Override
    public String asString() {
        final List<String> names = new ListOf<>(
            new Mapped<>(loc -> loc.replaceFirst("^Φ\\.", ""), this.busy)
        );
        final StringBuilder line = new StringBuilder(
            Logger.format(
                "%d/%d entries in %[ms]s, %d from cache, %[size]s in XMLs",
                this.done.get(),
                this.total,
                System.currentTimeMillis() - this.start,
                this.reused.get(),
                this.bytes.get()
            )
        );
        if (!names.isEmpty()) {
            line.append(": ").append(String.join(", ", new HeadOf<>(Progress.SHOWN, names)));
        }
        if (names.size() > Progress.SHOWN) {
            line.append(", +").append(names.size() - Progress.SHOWN);
        }
        return line.toString();
    }

    /**
     * Count one more entry as in processing.
     *
     * @param locator The locator of the entry, such as {@code Φ.number.exp}
     */
    void begin(final String locator) {
        this.busy.add(locator);
    }

    /**
     * Count one entry as not in processing any more.
     *
     * @param locator The locator of the entry, such as {@code Φ.number.exp}
     */
    void end(final String locator) {
        this.busy.remove(locator);
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
