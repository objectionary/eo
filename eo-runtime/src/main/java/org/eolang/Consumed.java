/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.management.ThreadMXBean;
import java.lang.management.ManagementFactory;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * How much memory the threads of a single group have taken.
 *
 * <p>The JVM keeps no tally of the heap that one test holds: the heap is
 * shared by all of them at once and the collector walks it on a clock of
 * its own. What the JVM does keep is how many bytes every thread has
 * allocated since it started, and that is what is counted here, for the
 * thread a test runs on and for every thread the test starts, since a new
 * thread is born into the group of the one that made it. Garbage counts
 * too: a test that pushes a gigabyte through the collector is as much of a
 * danger to the build as one that holds a gigabyte to the very end.</p>
 *
 * <p>A thread that is already gone answers with a negative number and would
 * take its share of the total away with it. The largest reading taken while
 * it was still alive stays in the map instead, so a test is charged for the
 * threads it started and left behind too. This is also why a thread about
 * to end takes a reading of itself before it goes, rather than leaving the
 * last word to a watcher that may look a moment too late.</p>
 *
 * @since 0.75.0
 */
final class Consumed {

    /**
     * The bean that counts bytes, empty if this JVM refuses to count them.
     */
    private static final Optional<ThreadMXBean> BEAN = Consumed.bean();

    /**
     * The group whose threads are counted.
     */
    private final ThreadGroup group;

    /**
     * The largest reading taken from every thread, by thread identifier.
     *
     * <p>Both the thread that runs the test and the one that watches it
     * take readings, so the map is a concurrent one and a reading only ever
     * replaces a smaller one: the counter of a thread never goes down, and
     * a reading that arrives late must not undo a fresher one.</p>
     */
    private final Map<Long, Long> readings;

    /**
     * Ctor.
     *
     * @param grp The group whose threads are counted
     */
    Consumed(final ThreadGroup grp) {
        this.group = grp;
        this.readings = new ConcurrentHashMap<>(0);
    }

    /**
     * Does this JVM count the bytes a thread allocates?
     *
     * @return TRUE if it does, FALSE if there is nothing to count with
     */
    static boolean counting() {
        return Consumed.BEAN.isPresent();
    }

    /**
     * How many bytes the group has allocated so far.
     *
     * @return Bytes allocated, or zero if this JVM does not count them
     */
    long bytes() {
        this.refresh();
        long total = 0L;
        for (final long taken : this.readings.values()) {
            total += taken;
        }
        return total;
    }

    /**
     * Take a fresh reading from every thread of the group.
     */
    void refresh() {
        final Thread[] threads = this.threads();
        if (Consumed.counting() && threads.length > 0) {
            final long[] ids = new long[threads.length];
            for (int idx = 0; idx < threads.length; ++idx) {
                ids[idx] = threads[idx].getId();
            }
            final long[] taken = Consumed.BEAN.get().getThreadAllocatedBytes(ids);
            for (int idx = 0; idx < ids.length; ++idx) {
                if (taken[idx] >= 0L) {
                    this.readings.merge(ids[idx], taken[idx], Math::max);
                }
            }
        }
    }

    private Thread[] threads() {
        int room = 1 + this.group.activeCount() * 2;
        Thread[] found = new Thread[room];
        int count = this.group.enumerate(found, true);
        while (count == found.length) {
            room *= 2;
            found = new Thread[room];
            count = this.group.enumerate(found, true);
        }
        return Arrays.copyOf(found, count);
    }

    private static Optional<ThreadMXBean> bean() {
        final Optional<ThreadMXBean> found;
        final Object mbean = ManagementFactory.getThreadMXBean();
        if (mbean instanceof ThreadMXBean bean && bean.isThreadAllocatedMemorySupported()) {
            bean.setThreadAllocatedMemoryEnabled(true);
            found = Optional.of(bean);
        } else {
            found = Optional.empty();
        }
        return found;
    }
}
