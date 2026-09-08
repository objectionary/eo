/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.extension.InvocationInterceptor;
import org.opentest4j.TestAbortedException;

/**
 * A body of a test that may allocate no more than the memory it is given.
 *
 * <p>The body runs in a thread of its own, born into a group of its own, so
 * that the threads it starts land in the same group and are counted with
 * it. While it runs, the group is asked every few milliseconds how much it
 * has allocated. The moment the answer is over the limit, the group is
 * interrupted and given a bounded grace to actually stop: dataization gives
 * up on the very next attribute lookup of an interrupted thread (see
 * {@link ExInterrupted}), so the objects it was building become garbage and
 * the heap comes back to the tests that still need it, once the wait is
 * over.</p>
 *
 * <p>The wait is the whole point of the guard and not a courtesy. A skip
 * reported while the body still runs gives the heap back to nobody: JUnit
 * hands the slot to the next test while the body keeps allocating, and the
 * {@code OutOfMemoryError} lands minutes later in a test that ate nothing
 * (#8336). So the group is interrupted on every turn of the wait, since one
 * interrupt is lost the moment anything swallows the
 * {@link InterruptedException} it raises, and the guard returns only once
 * the body is out or the five seconds it is given run out. Either way the
 * test is reported as skipped, and the message says which of the two
 * happened: a body that would not stop is worth naming, but it is not
 * worth failing a test over, since a budget a bigger box would never have
 * reached says nothing about the code under test whether the body noticed
 * the interrupt or not. The budget binds one test at a time, so it bounds
 * the heap only while the budget times the workers JUnit is given stays
 * under {@code -Xmx}.</p>
 *
 * <p>A body that ends between two readings is judged all the same, on the
 * reading it took of itself on the way out. A body that failed, though,
 * fails the test with its own problem, whatever it ate: a broken test is
 * worth more than a tidy skip.</p>
 *
 * <p>The watching thread is the one JUnit runs the test on, so it is also
 * the one JUnit interrupts when the test outlives {@code eo.deadline}: the
 * timeout of a test is scheduled against the thread that enters the
 * interceptor, not against the thread the body ends up on. That interrupt
 * therefore has to be carried over to the group by hand. Without it the
 * deadline stops watching and nothing else, the body is left running on a
 * thread nobody waits for any more, and a suite where many tests outlive
 * their second ends up with as many runaway threads, all of them
 * allocating, until the heap is gone.</p>
 *
 * <p>The test itself is reported as skipped rather than as failed, for the
 * same reason {@link Deadline} reports a slow one as skipped: a budget that
 * a box with more memory would never have reached says nothing about the
 * code under test.</p>
 *
 * @since 0.75.0
 * @todo #8336:30min Wait for the threads a body left behind too. They are
 *  interrupted as often as the body is, but never waited for, so one may
 *  still hold a file when JUnit deletes the {@code @TempDir} of the test
 *  behind it, which on windows fails. Wait for the group to empty as well,
 *  or say plainly which threads outlived the test.
 */
@SuppressWarnings({"PMD.AvoidThreadGroup", "PMD.AvoidCatchingGenericException"})
final class Watched {

    /**
     * What is said about a test that ate more than it was given.
     */
    private static final String MESSAGE = String.join(
        " ",
        "The test allocated %d bytes, which is over the %d bytes",
        "of eo.maxmem it was given, so it was terminated%s"
    );

    /**
     * What is added when the body of a test would not stop.
     */
    private static final String OUTLIVED = String.join(
        " ",
        ", and it would not stop within %d milliseconds of being",
        "interrupted, so its thread may still be holding the heap"
    );

    /**
     * How many tests have been watched, to give every group its own name.
     */
    private static final AtomicLong COUNT = new AtomicLong();

    /**
     * How many bytes the body may allocate, or zero if it may allocate all.
     */
    private final long limit;

    /**
     * How long a terminated body is given to stop, in milliseconds.
     */
    private final long grace;

    /**
     * Ctor.
     * @param bytes How many bytes the body may allocate, zero for no limit
     */
    Watched(final long bytes) {
        this(bytes, 5_000L);
    }

    /**
     * Ctor.
     * @param bytes How many bytes the body may allocate, zero for no limit
     * @param millis How long a terminated body is given to stop
     */
    Watched(final long bytes, final long millis) {
        this.limit = bytes;
        this.grace = millis;
    }

    // @checkstyle IllegalThrowsCheck (13 lines)
    /**
     * Run the body and let it allocate no more than the limit.
     *
     * <p>A test runs on the very thread that called this method when there
     * is no limit to keep, or when the JVM does not count what a thread
     * allocates: watching would cost a thread and buy nothing.</p>
     *
     * @param body The body of the test
     * @throws Throwable If the body fails, or if it eats too much
     */
    void through(final InvocationInterceptor.Invocation<Void> body) throws Throwable {
        if (this.limit <= 0L || !Consumed.counting()) {
            body.proceed();
        } else {
            this.guarded(body);
        }
    }

    // @checkstyle IllegalThrowsCheck (39 lines)
    private void guarded(final InvocationInterceptor.Invocation<Void> body) throws Throwable {
        final ThreadGroup group = new ThreadGroup(
            String.format("maxmem-%d", Watched.COUNT.incrementAndGet())
        );
        final AtomicReference<Throwable> failure = new AtomicReference<>();
        final CountDownLatch done = new CountDownLatch(1);
        final Consumed consumed = new Consumed(group);
        final Thread thread = new Thread(
            group,
            () -> Watched.run(body, failure, done, consumed)
        );
        thread.setDaemon(true);
        thread.start();
        boolean over = false;
        try {
            while (!done.await(50L, TimeUnit.MILLISECONDS)) {
                if (consumed.bytes() > this.limit) {
                    over = true;
                    break;
                }
            }
        } catch (final InterruptedException ex) {
            this.stopped(group, done);
            throw ex;
        }
        if (over) {
            throw this.aborted(consumed, this.stopped(group, done));
        }
        final Throwable error = failure.get();
        if (error != null) {
            throw error;
        }
        if (consumed.bytes() > this.limit) {
            throw this.aborted(consumed, true);
        }
    }

    private boolean stopped(final ThreadGroup group, final CountDownLatch done) {
        final long deadline = System.currentTimeMillis() + this.grace;
        group.interrupt();
        while (done.getCount() > 0L && System.currentTimeMillis() < deadline) {
            try {
                done.await(50L, TimeUnit.MILLISECONDS);
            } catch (final InterruptedException ex) {
                Thread.currentThread().interrupt();
                break;
            }
            group.interrupt();
        }
        return done.getCount() == 0L;
    }

    private TestAbortedException aborted(final Consumed consumed, final boolean gone) {
        final String tail;
        if (gone) {
            tail = "";
        } else {
            tail = String.format(Watched.OUTLIVED, this.grace);
        }
        return new TestAbortedException(
            String.format(Watched.MESSAGE, consumed.bytes(), this.limit, tail)
        );
    }

    // @checkstyle IllegalCatchCheck (12 lines)
    private static void run(final InvocationInterceptor.Invocation<Void> body,
        final AtomicReference<Throwable> failure, final CountDownLatch done,
        final Consumed consumed) {
        try {
            body.proceed();
        } catch (final Throwable error) {
            failure.set(error);
        } finally {
            consumed.refresh();
            done.countDown();
        }
    }
}
