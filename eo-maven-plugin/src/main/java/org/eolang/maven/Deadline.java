/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
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
     * @param body The body of the Mojo
     * @throws MojoFailureException If the deadline passes or the body fails
     */
    @SuppressWarnings("PMD.CloseResource")
    void spent(final Callable<?> body) throws MojoFailureException {
        final ExecutorService service = Executors.newSingleThreadExecutor();
        try {
            service.submit(body).get(this.seconds, TimeUnit.SECONDS);
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
            Deadline.stopped(service);
        }
    }

    /**
     * Turn the problem into a Maven failure, logging its causes first.
     * @param msg The message for the failure
     * @param problem The problem itself
     * @throws MojoFailureException For sure
     */
    private void reported(final String msg, final Throwable problem)
        throws MojoFailureException {
        if (this.unroll) {
            for (final String cause : new Causes(problem)) {
                Logger.error(this.mojo, cause);
            }
        }
        throw new MojoFailureException(msg, problem);
    }

    /**
     * Wait for the service to stop, whatever it takes.
     * @param service The service that ran the body
     */
    private static void stopped(final ExecutorService service) {
        boolean terminated = false;
        service.shutdown();
        while (!terminated) {
            try {
                terminated = service.awaitTermination(60, TimeUnit.SECONDS);
                if (terminated) {
                    service.shutdownNow();
                }
            } catch (final InterruptedException ex) {
                service.shutdownNow();
                Thread.currentThread().interrupt();
            }
        }
    }
}
