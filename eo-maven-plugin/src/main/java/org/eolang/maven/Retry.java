/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import org.cactoos.Scalar;
import org.cactoos.scalar.IoChecked;

/**
 * Retry of delegate scalar.
 *
 * @param <T> Type
 * @since 0.59.0
 */
public final class Retry<T> implements Scalar<T> {
    /**
     * The delegate scalar.
     */
    private final Scalar<? extends T> origin;

    /**
     * Count of retrys possible.
     */
    private final Integer count;

    /**
     * Main ctor.
     *
     * @param origin Scalar
     * @param count Retry count
     */
    public Retry(final Scalar<? extends T> origin, final Integer count) {
        this.origin = origin;
        this.count = count;
    }

    @Override
    public T value() throws IOException {
        return this.tried(1);
    }

    /**
     * Recursive retry func.
     *
     * @param tried Already tryied
     * @return Res
     * @throws IOException If reached retry limit.
     */
    private T tried(final Integer tried) throws IOException {
        T res;
        try {
            res = new IoChecked<>(this.origin).value();
        } catch (final IOException exception) {
            if (tried < this.count) {
                Logger.debug(
                    this,
                    "Failed to execute scalar delegate. Try '%s' of '%s' trys",
                    tried, this.count
                );
                res = this.tried(tried + 1);
            } else {
                throw new IOException(
                    String.format(
                        "Failed to execute scalar delegate after %s trys",
                        tried
                    ),
                    exception
                );
            }
        }
        return res;
    }
}
