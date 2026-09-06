/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * A body of a Mojo that must finish before its deadline.
 *
 * <p>The body runs in a thread of its own, so that one Mojo which never
 * returns does not hang the whole build for good. When the deadline passes,
 * or the body fails, the problem becomes a Maven failure, with the chain of
 * causes in the log if the build asks for it.</p>
 *
 * @since 0.62.0
 */
final class Deadline {

    /**
     * The Mojo the body belongs to, also the source of the log records.
     */
    private final Object mojo;

    /**
     * How many seconds the body may take.
     */
    private final long seconds;

    /**
     * Whether the chain of causes goes to the log.
     */
    private final boolean unroll;

    /**
     * Ctor.
     * @param mojo The Mojo the body belongs to
     * @param seconds How many seconds the body may take
     * @param unroll Whether the chain of causes goes to the log
     */
    Deadline(final Object mojo, final long seconds, final boolean unroll) {
        this.mojo = mojo;
        this.seconds = seconds;
        this.unroll = unroll;
    }

    /**
     * Run the body and wait for it, but no longer than the deadline.
     *
     * <p>The body goes into a {@link FutureTask} carried by a daemon
     * thread of its own, rather than into an executor service. A cancel
     * only interrupts the running task, and a task blocked on something
     * that ignores interruption (a tight CPU loop, a blocking socket read)
     * may never terminate. An executor would then have to be shut down and
     * waited for, and that wait would either hang this method forever -
     * silently swallowing whatever {@link MojoFailureException} the
     * deadline itself just raised - or give up after a bounded time and
     * leave the thread running anyway. A daemon thread needs neither: it
     * is abandoned where it stands and never holds the JVM back from
     * exiting.</p>
     *
     * @param body The body of the Mojo
     * @throws MojoFailureException If the deadline passes or the body fails
     */
    void spent(final Callable<?> body) throws MojoFailureException {
        if (this.seconds < 0L) {
            throw new IllegalArgumentException(
                String.format(
                    "The timeout must not be negative, while %d was given through eo.timeout",
                    this.seconds
                )
            );
        }
        final FutureTask<?> task = new FutureTask<>(body);
        final Thread thread = new Thread(
            task,
            String.format("%s-deadline", this.mojo.getClass().getSimpleName())
        );
        thread.setDaemon(true);
        thread.start();
        try {
            task.get(this.seconds, TimeUnit.SECONDS);
        } catch (final TimeoutException ex) {
            this.reported(
                Logger.format(
                    "Timeout %[ms]s for Mojo execution is reached",
                    TimeUnit.SECONDS.toMillis(this.seconds)
                ),
                ex
            );
        } catch (final ExecutionException ex) {
            this.reported(String.format("'%s' execution failed", this.mojo), ex);
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(
                Logger.format(
                    "Timeout %[ms]s thread was interrupted",
                    TimeUnit.SECONDS.toMillis(this.seconds)
                ),
                ex
            );
        } finally {
            task.cancel(true);
        }
    }

    private void reported(final String msg, final Throwable problem)
        throws MojoFailureException {
        if (this.unroll) {
            for (final String cause : new Causes(problem)) {
                Logger.error(this.mojo, cause);
            }
        }
        throw new MojoFailureException(msg, problem);
    }
}
