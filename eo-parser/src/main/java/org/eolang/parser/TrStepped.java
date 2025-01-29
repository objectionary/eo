/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
