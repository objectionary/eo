/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Timeout for execution thread.
 *
 * @since 0.28.12
 */
final class Timeout {

    /**
     * Timeout value that determines how long to wait before interrupt.
     */
    private final int value;

    /**
     * Timeout units.
     */
    private final TimeUnit unit;

    /**
     * Thread to interrupt if timeout limit reached.
     */
    private final Thread thread;

    /**
     * Inner flag that purpose to determine if timeout have to interrupt a thread.
     */
    private final AtomicBoolean finish;

    /**
     * Synchronization lock.
     */
    private final Object lock;

    /**
     * The shortest constructor with default preset for units and thread.
     *
     * @param value How long to wait before interrupt.
     */
    Timeout(final int value) {
        this(value, Thread.currentThread());
    }

    /**
     * Simple constructor with timeout and time units.
     * Useful for tests.
     *
     * @param value How long to wait before interrupt.
     * @param unit Time units
     */
    Timeout(final int value, final TimeUnit unit) {
        this(value, unit, Thread.currentThread());
    }

    /**
     * Simple constructor with timeout and thread.
     *
     * @param value How long to wait before interrupt.
     * @param thread To interrupt if timeout is reached.
     */
    private Timeout(final int value, final Thread thread) {
        this(value, TimeUnit.SECONDS, thread);
    }

    /**
     * The main constructor.
     *
     * @param value How long to wait before interrupt.
     * @param unit Time units
     * @param thread To interrupt if timeout is reached.
     */
    private Timeout(final int value, final TimeUnit unit, final Thread thread) {
        this.value = value;
        this.unit = unit;
        this.thread = thread;
        this.finish = new AtomicBoolean(false);
        this.lock = new Object();
    }

    /**
     * Starts counting down before interrupting a thread.
     * If timeout limit is reached the thread will be interrupted.
     */
    void start() {
        synchronized (this.lock) {
            final ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
            this.finish.set(false);
            executor.schedule(this::interrupt, this.value, this.unit);
            executor.shutdown();
        }
    }

    /**
     * Stops counting down before interrupting a thread.
     * If that method was call before timeout limit is reached then nothing will happen and
     * normal execution will continue.
     */
    void stop() {
        synchronized (this.lock) {
            this.finish.set(true);
        }
    }

    /**
     * Interrupts thread if timeout wasn't sopped.
     */
    private void interrupt() {
        synchronized (this.lock) {
            if (!this.finish.get()) {
                this.thread.interrupt();
                Logger.warn(
                    this,
                    String.format(
                        "Timeout ('%d %s') is reached for thread '%s'",
                        this.value,
                        this.unit,
                        this.thread
                    )
                );
            }
        }
    }
}
