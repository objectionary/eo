/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.opentest4j.TestAbortedException;

/**
 * Test case for {@link Watched}.
 * @since 0.75.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class WatchedTest {

    @Test
    void terminatesBodyThatKeepsAllocating() {
        MatcherAssert.assertThat(
            "A body that never stops allocating must be terminated and interrupted, but it wasnt",
            WatchedTest.abandoned(),
            Matchers.is(true)
        );
    }

    @Test
    void stopsBodyBeforeGuardReturns() {
        final AtomicBoolean stopped = new AtomicBoolean(false);
        Assertions.assertThrows(
            TestAbortedException.class,
            () -> new Watched(1024L * 1024L).through(
                () -> {
                    final byte[][] junk = new byte[1][];
                    while (!Thread.currentThread().isInterrupted()) {
                        junk[0] = new byte[256 * 1024];
                        WatchedTest.rest(5L);
                    }
                    stopped.set(true);
                    return null;
                }
            ),
            "A body that ate too much must be terminated, but it wasnt"
        );
        MatcherAssert.assertThat(
            "A body must already be stopped by the time the guard returns, but it wasnt",
            stopped.get(),
            Matchers.is(true)
        );
    }

    @Test
    void stopsBodyWhenTheWatcherIsInterrupted() {
        MatcherAssert.assertThat(
            "A body must be stopped when the thread watching it is interrupted, but it wasnt",
            WatchedTest.interrupted(),
            Matchers.is(true)
        );
    }

    @Test
    void skipsBodyThatAteTooMuchAndFinished() {
        Assertions.assertThrows(
            TestAbortedException.class,
            () -> new Watched(1024L * 1024L).through(
                () -> {
                    final byte[][] junk = new byte[1][];
                    for (int idx = 0; idx < 16; ++idx) {
                        junk[0] = new byte[256 * 1024];
                    }
                    return null;
                }
            ),
            "A body that ate more than it was given must be reported as skipped, but it wasnt"
        );
    }

    @Test
    @Timeout(value = 10L, unit = TimeUnit.SECONDS)
    void refusesToSkipBodyThatWillNotStop() {
        MatcherAssert.assertThat(
            "A body that will not stop must be said to outlive its test, but it wasnt",
            WatchedTest.lingering(),
            Matchers.containsString("outlived the test")
        );
    }

    @Test
    void letsFrugalBodyThrough() {
        MatcherAssert.assertThat(
            "A body that eats almost nothing must run to its end, but it didnt",
            WatchedTest.where(64L * 1024L * 1024L),
            Matchers.notNullValue()
        );
    }

    @Test
    void letsFailureOfBodyThrough() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Watched(64L * 1024L * 1024L).through(
                () -> {
                    throw new IllegalStateException("it broke");
                }
            ),
            "A body that failed must fail the test with its own problem, but it didnt"
        );
    }

    @Test
    void watchesNothingWithoutLimit() {
        MatcherAssert.assertThat(
            "With no limit the body must run on the very thread that called it, but it didnt",
            WatchedTest.where(0L),
            Matchers.sameInstance(Thread.currentThread())
        );
    }

    private static boolean abandoned() {
        final AtomicBoolean stopped = new AtomicBoolean(false);
        Assertions.assertThrows(
            TestAbortedException.class,
            () -> new Watched(1024L * 1024L).through(
                () -> {
                    final byte[][] junk = new byte[1][];
                    while (!Thread.currentThread().isInterrupted()) {
                        junk[0] = new byte[256 * 1024];
                        WatchedTest.rest(1L);
                    }
                    stopped.set(true);
                    return null;
                }
            ),
            "A body that never stops allocating must be terminated, but it wasnt"
        );
        return WatchedTest.awaited(stopped);
    }

    private static String lingering() {
        final AtomicBoolean release = new AtomicBoolean(false);
        try {
            return Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Watched(1024L * 1024L).through(
                    () -> {
                        final byte[][] junk = new byte[1][];
                        for (int idx = 0; idx < 16; ++idx) {
                            junk[0] = new byte[256 * 1024];
                        }
                        while (!release.get()) {
                            WatchedTest.rest(1L);
                        }
                        return null;
                    }
                ),
                "A body that ignores its interrupt must fail the test, but it didnt"
            ).getMessage();
        } finally {
            release.set(true);
        }
    }

    private static boolean interrupted() {
        final AtomicBoolean stopped = new AtomicBoolean(false);
        final Thread watcher = Thread.currentThread();
        final Thread bell = new Thread(
            () -> {
                WatchedTest.rest(100L);
                watcher.interrupt();
            }
        );
        bell.setDaemon(true);
        bell.start();
        Assertions.assertThrows(
            InterruptedException.class,
            () -> new Watched(64L * 1024L * 1024L).through(
                () -> {
                    while (!Thread.currentThread().isInterrupted()) {
                        WatchedTest.rest(1L);
                    }
                    stopped.set(true);
                    return null;
                }
            ),
            "The interrupt of the watching thread must be reported, but it wasnt"
        );
        return WatchedTest.awaited(stopped);
    }

    private static Thread where(final long limit) {
        final AtomicReference<Thread> ran = new AtomicReference<>();
        Assertions.assertDoesNotThrow(
            () -> new Watched(limit).through(
                () -> {
                    ran.set(Thread.currentThread());
                    return null;
                }
            ),
            "A body that eats almost nothing must not be touched, but it was"
        );
        return ran.get();
    }

    private static void rest(final long millis) {
        try {
            Thread.sleep(millis);
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
        }
    }

    private static boolean awaited(final AtomicBoolean flag) {
        final long deadline = System.currentTimeMillis() + 500L;
        while (!flag.get() && System.currentTimeMillis() < deadline) {
            WatchedTest.rest(1L);
        }
        return flag.get();
    }
}
