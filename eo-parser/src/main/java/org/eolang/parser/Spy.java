/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import java.io.IOException;

/**
 * Spy to use in {@link Xsline}.
 *
 * @since 0.1
 */
public interface Spy {

    /**
     * New XSL produced.
     *
     * @param index The index of the XSL
     * @param xsl The XSL used
     * @param xml The XML produced
     * @throws IOException If fails
     */
    void push(int index, XSL xsl, XML xml) throws IOException;

    /**
     * Empty spy.
     *
     * @since 0.1
     */
    final class None implements Spy {
        @Override
        public void push(final int index, final XSL xsl, final XML xml) {
            // Nothing
        }
    }

    /**
     * Verbose spy, printing the progress to the console.
     *
     * @since 0.1
     */
    final class Verbose implements Spy {
        /**
         * The logging target.
         */
        private final Object target;

        /**
         * Default ctor.
         */
        public Verbose() {
            this(Verbose.class);
        }

        /**
         * Ctor.
         * @param tgt The logging target to use
         */
        public Verbose(final Object tgt) {
            this.target = tgt;
        }

        @Override
        public void push(final int index, final XSL xsl, final XML xml) {
            Logger.debug(
                this.target,
                "Parsed #%d via %s\n%s",
                index,
                new XMLDocument(xsl.toString()).xpath("/*/@id").get(0),
                xml
            );
        }
    }
}
