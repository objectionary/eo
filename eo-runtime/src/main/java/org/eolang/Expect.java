/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * This wrapper helps us explain our expectations in an error
 * message that we throw.
 *
 * @param <T> The type of result
 * @since 0.41.0
 */
public class Expect<T> {

    /**
     * The subject being tested.
     */
    private final String subject;

    /**
     * The supplier.
     */
    private final Supplier<T> sup;

    /**
     * Ctor.
     *
     * @param subj The subject
     * @param supplier The supplier
     */
    public Expect(final String subj, final Supplier<T> supplier) {
        this.subject = subj;
        this.sup = supplier;
    }

    /**
     * Starting point.
     *
     * @param phi The object
     * @param attr Attribute name
     * @return Expect pipeline
     */
    @SuppressWarnings("PMD.ProhibitPublicStaticMethods")
    public static Expect<Phi> at(final Phi phi, final String attr) {
        return new Expect<>(
            String.format("the '%s' attribute", attr),
            () -> phi.take(attr)
        );
    }

    /**
     * Assert that it passes.
     *
     * @param fun The function to transform
     * @param <R> Type of result
     * @return New object
     */
    public <R> Expect<R> that(final Function<T, R> fun) {
        return new Expect<>(
            this.subject,
            () -> {
                try {
                    return fun.apply(this.sup.get());
                } catch (final ExFailure ex) {
                    throw new ExThat(ex.getMessage(), ex);
                }
            }
        );
    }

    /**
     * Fail with this message otherwise.
     *
     * @param message The error message
     * @return Next object
     */
    public Expect<T> otherwise(final String message) {
        return new Expect<>(
            this.subject,
            () -> this.applyOtherwise(message)
        );
    }

    /**
     * Assert on it.
     *
     * @param fun The check
     * @return Next object
     */
    public Expect<T> must(final Function<T, Boolean> fun) {
        return new Expect<>(
            this.subject,
            () -> {
                final T ret = this.sup.get();
                if (!fun.apply(ret)) {
                    throw new ExMust(
                        String.format("(%s)", ret)
                    );
                }
                return ret;
            }
        );
    }

    /**
     * Return it.
     *
     * @return The token
     */
    public T it() {
        try {
            return this.sup.get();
        } catch (final ExOtherwise ex) {
            throw new ExFailure(ex.getMessage(), ex);
        }
    }

    private T applyOtherwise(final String message) {
        try {
            return this.sup.get();
        } catch (final ExMust ex) {
            throw new ExOtherwise(
                String.format(
                    "%s %s %s",
                    this.subject,
                    ex.getMessage(),
                    message
                ),
                ex
            );
        } catch (final ExThat ex) {
            throw new ExOtherwise(
                String.format(
                    "%s %s",
                    this.subject,
                    message
                ),
                ex
            );
        }
    }
}
