/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.maven.log;

import com.jcabi.log.Logger;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Captured logs.
 *
 * @since 0.30
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
public final class Logs {

    /**
     * Captured logs.
     */
    private final Collection<String> container;

    /**
     * Ctor.
     */
    Logs() {
        this(new ConcurrentLinkedQueue<>());
    }

    /**
     * Ctor.
     * @param collection Captured logs.
     */
    private Logs(final Collection<String> collection) {
        this.container = collection;
    }

    /**
     * Return all captured logs up to this point.
     * @return Captured logs.
     */
    public Collection<String> captured() {
        return Collections.unmodifiableCollection(this.container);
    }

    /**
     * Since logging is usually asynchronous, we need to wait for the message to appear in the
     * output. Moreover, logging system can take extra time to initialize.
     * @param message Expected part of the message
     * @return Logged message with formatting
     */
    public String waitForMessage(final String message) {
        return this.waitForMessage(
            message,
            () -> {
            }
        );
    }

    /**
     * Waits until the logger is initialized.
     * If that method is finished it means that loggers are initialized.
     */
    void waitForInit() {
        final String msg = "ping";
        this.waitForMessage(msg, () -> Logger.info(this, msg));
    }

    /**
     * Append log message.
     * @param log Log message.
     */
    void append(final String log) {
        this.container.add(log);
    }

    /**
     * Wait for message.
     * @param message Message to wait for
     * @param action Action to perform before checking the message on each check iteration
     * @return Logged message
     */
    private String waitForMessage(final String message, final Runnable action) {
        try {
            return Executors.newSingleThreadExecutor().submit(
                () -> {
                    while (true) {
                        action.run();
                        final Optional<String> full = this.container.stream()
                            .filter(s -> s.contains(message))
                            .findFirst();
                        if (full.isPresent()) {
                            return full.get();
                        }
                    }
                }
            ).get(10, TimeUnit.SECONDS);
        } catch (final InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(
                String.format(
                    "Waiting thread was interrupted, can't read '%s' msg",
                    message
                ),
                exception
            );
        } catch (final ExecutionException exception) {
            throw new IllegalStateException(
                String.format(
                    "Some problem happened, can't read '%s' msg",
                    message
                ),
                exception
            );
        } catch (final TimeoutException exception) {
            throw new IllegalStateException(
                String.format(
                    "Timeout limit exceed to read msg %s, current set of captured logs: %s",
                    message,
                    this.container
                ),
                exception
            );
        }
    }
}
