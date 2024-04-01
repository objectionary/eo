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

/*
 * @checkstyle PackageNameCheck (4 lines)
 */

package EOorg.EOeolang;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import org.eolang.AtSimple;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.PhWrite;
import org.eolang.Phi;
import org.eolang.Term;
import org.eolang.XmirObject;

/**
 * Memory.alloc object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "memory.alloc")
public final class EOmemory$EOalloc extends PhDefault implements Atom {
    /**
     * Locator calculator.
     */
    private static final AtomicLong LOCATOR = new AtomicLong(0L);

    /**
     * Memory.
     */
    private static final ConcurrentHashMap<Long, Phi> MEMORY = new ConcurrentHashMap<>(0);

    /**
     * Ctor.
     * @param sigma Sigma
     */
    EOmemory$EOalloc(final Phi sigma) {
        super(sigma);
        this.add("data", new EOmemory$EOalloc.AtMalloc());
        this.add("write", new AtSimple(new PhWrite(this, "data")));
    }

    @Override
    public Phi lambda() throws Exception {
        return this.take("data");
    }

    /**
     * Attribute that allocates data in memory.
     * @since 0.36.0
     */
    private static class AtMalloc implements Attr {
        /**
         * Object in memory.
         */
        private Long locator;

        /**
         * Allocated bytes length.
         */
        private Integer length;

        /**
         * Ctor.
         */
        AtMalloc() {
            this(null, null);
        }

        /**
         * Ctor for copying.
         * @param locator Locator of object in memory
         * @param length  Allocated bytes length
         */
        AtMalloc(final Long locator, final Integer length) {
            this.locator = locator;
            this.length = length;
        }

        @Override
        public Attr copy(final Phi self) {
            return new EOmemory$EOalloc.AtMalloc(this.locator, this.length);
        }

        @Override
        public Phi get() {
            if (this.locator == null || !EOmemory$EOalloc.MEMORY.containsKey(this.locator)) {
                throw new ExFailure(
                    "Current memory segment is empty, can't read it"
                );
            }
            return EOmemory$EOalloc.MEMORY.get(this.locator);
        }

        @Override
        public void put(final Phi phi) {
            final byte[] bytes = new Dataized(phi).take();
            if (this.length == null) {
                this.length = bytes.length;
            } else if (this.length < bytes.length) {
                throw new ExFailure(
                    "Can't write to memory %d bytes because %d were already allocated",
                    bytes.length,
                    this.length
                );
            }
            if (this.locator == null) {
                synchronized (EOmemory$EOalloc.LOCATOR) {
                    this.locator = EOmemory$EOalloc.LOCATOR.incrementAndGet();
                }
            }
            EOmemory$EOalloc.MEMORY.put(this.locator, new Data.ToPhi(bytes));
        }

        @Override
        public String φTerm() {
            final String txt;
            if (this.locator == null || !EOmemory$EOalloc.MEMORY.containsKey(this.locator)) {
                txt = Term.EMPTY;
            } else {
                txt = EOmemory$EOalloc.MEMORY.get(this.locator).φTerm();
            }
            return txt;
        }

        @Override
        public String toString() {
            final String txt;
            if (this.locator == null || !EOmemory$EOalloc.MEMORY.containsKey(this.locator)) {
                txt = Term.EMPTY;
            } else {
                txt = EOmemory$EOalloc.MEMORY.get(this.locator).toString();
            }
            return txt;
        }
    }
}