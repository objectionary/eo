/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StAfter;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;
import java.util.concurrent.CountDownLatch;
import org.cactoos.Scalar;
import org.cactoos.io.ResourceOf;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.text.TextOf;

/**
 * Train that adds sheet names that were processed.
 *
 * @since 0.1
 */
final class TrStepped extends TrEnvelope {

    /**
     * Apply changes to each XML after processing.
     */
    private static final Scalar<XSL> STEPPED = new Sticky<>(
        new Once<XSL>(
            () -> new XSLDocument(
                new TextOf(
                    new ResourceOf("org/eolang/parser/_stepped.xsl")
                ).asString()
            )
        )
    );

    /**
     * Ctor.
     *
     * @param train Original train
     */
    TrStepped(final Train<Shift> train) {
        this(train, TrStepped.STEPPED);
    }

    /**
     * Ctor.
     *
     * @param train Original train
     * @param stepped XSL to apply
     */
    TrStepped(final Train<Shift> train, final Scalar<XSL> stepped) {
        super(
            new TrLambda(
                train,
                shift -> new StAfter(
                    shift,
                    new StLambda(
                        shift::uid,
                        (pos, xml) -> new Synced<>(stepped).value()
                            .with("step", pos)
                            .with("sheet", shift.uid())
                            .transform(xml)
                    )
                )
            )
        );
    }

    /**
     * Scalar that loads the value only once.
     *
     * @param <T> Type of the value
     * @since 0.51
     */
    static final class Once<T> implements Scalar<T> {

        /**
         * Origin scalar.
         */
        private final Scalar<T> origin;

        /**
         * Latch to count down.
         */
        private final CountDownLatch latch;

        /**
         * Ctor.
         *
         * @param origin Origin scalar
         */
        Once(final Scalar<T> origin) {
            this(origin, new CountDownLatch(1));
        }

        /**
         * Ctor.
         *
         * @param origin Origin scalar
         * @param latch Latch to count down
         */
        private Once(final Scalar<T> origin, final CountDownLatch latch) {
            this.origin = origin;
            this.latch = latch;
        }

        @Override
        public T value() throws Exception {
            if (this.latch.getCount() < 1) {
                throw new IllegalStateException(
                    String.format("Resource '%s' should be loaded only once", this.origin)
                );
            }
            this.latch.countDown();
            return this.origin.value();
        }
    }
}
