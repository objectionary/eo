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
@SuppressWarnings("PMD.ShortMethodName")
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
     * @param subj The subject
     * @param supplier The supplier
     */
    public Expect(final String subj, final Supplier<T> supplier) {
        this.subject = subj;
        this.sup = supplier;
    }

    /**
     * Starting point.
     * @param phi The object
     * @param attr Attribute name
     * @return Expect pipeline
     * @checkstyle MethodNameCheck (5 lines)
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
     * @param message The error message
     * @return Next object
     */
    public Expect<T> otherwise(final String message) {
        return new Expect<>(
            this.subject,
            () -> {
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
        );
    }

    /**
     * Assert on it.
     * @param fun The check.
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
     * @return The token
     * @checkstyle MethodNameCheck (5 lines)
     */
    public T it() {
        try {
            return this.sup.get();
        } catch (final ExOtherwise ex) {
            throw new ExFailure(ex.getMessage(), ex);
        }
    }

    /**
     * This exception is used to enhance the error message
     * in the {@link Expect#otherwise(String)} method.
     *
     * @since 0.51
     */
    private static final class ExMust extends RuntimeException {
        /**
         * Ctor.
         * @param cause Exception cause
         * @param args Arguments for {@link String#format(String, Object...)}
         */
        ExMust(final String cause, final Object... args) {
            super(String.format(cause, args));
        }
    }

    /**
     * This exception is used to enhance the error message
     * in the {@link Expect#otherwise(String)} method.
     *
     * @since 0.51
     */
    private static final class ExThat extends RuntimeException {
        /**
         * Ctor.
         * @param cause Exception cause
         * @param args Arguments for {@link String#format(String, Object...)}
         */
        ExThat(final String cause, final Object... args) {
            super(String.format(cause, args));
        }
    }

    /**
     * This exception is used to enhance the error message
     * in the {@link Expect#it()} method.
     *
     * @since 0.51
     */
    private static final class ExOtherwise extends RuntimeException {
        /**
         * Ctor.
         * @param cause Exception cause
         * @param args Arguments for {@link String#format(String, Object...)}
         */
        ExOtherwise(final String cause, final Object... args) {
            super(String.format(cause, args));
        }
    }

    /**
     * Transform Expect to Number.
     *
     * @since 0.51
     */
    public static final class Number {

        /**
         * Expect.
         */
        private final Expect<Phi> expect;

        /**
         * Ctor.
         * @param expect Expect
         */
        public Number(final Expect<Phi> expect) {
            this.expect = expect;
        }

        /**
         * Return it.
         * @return The token
         * @checkstyle MethodNameCheck (5 lines)
         */
        public Double it() {
            return this.expect
                .that(phi -> new Dataized(phi).asNumber())
                .otherwise("must be a number")
                .it();
        }
    }

    /**
     * Transform Expect to Integer.
     *
     * @since 0.51
     */
    public static final class Int {

        /**
         * Expect.
         */
        private final Expect<Phi> expect;

        /**
         * Ctor.
         * @param expect Expect
         */
        public Int(final Expect<Phi> expect) {
            this.expect = expect;
        }

        /**
         * Return it.
         * @return The token
         * @checkstyle MethodNameCheck (5 lines)
         */
        public Integer it() {
            return this.expect
                .that(phi -> new Dataized(phi).asNumber())
                .otherwise("must be a number")
                .must(number -> number % 1 == 0)
                .otherwise("must be an integer")
                .that(Double::intValue)
                .it();
        }
    }

    /**
     * Transform Expect to Natural number.
     * Natural number is integer greater or equal to zero.
     *
     * @since 0.51
     */
    public static final class Natural {

        /**
         * Expect.
         */
        private final Expect<Phi> expect;

        /**
         * Ctor.
         * @param expect Expect
         */
        public Natural(final Expect<Phi> expect) {
            this.expect = expect;
        }

        /**
         * Return it.
         * @return The token
         * @checkstyle MethodNameCheck (5 lines)
         */
        public Integer it() {
            return this.expect
                .that(phi -> new Dataized(phi).asNumber())
                .otherwise("must be a number")
                .must(number -> number % 1 == 0)
                .otherwise("must be an integer")
                .that(Double::intValue)
                .must(integer -> integer >= 0)
                .otherwise("must be greater or equal to zero")
                .it();
        }
    }

}
