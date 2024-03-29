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
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.PhWrite;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Cage.alloc object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "cage.alloc")
public final class EOcage$EOalloc extends PhDefault implements Atom {
    /**
     * Locator calculator.
     */
    private static final AtomicLong LOCATOR = new AtomicLong(0L);

    /**
     * Objects storage.
     */
    private static final ConcurrentHashMap<Long, Phi> OBJECTS = new ConcurrentHashMap<>(0);

    /**
     * Ctor.
     * @param sigma Sigma
     */
    EOcage$EOalloc(final Phi sigma) {
        super(sigma);
        this.add("object", new EOcage$EOalloc.AtEncaged());
        this.add("write", new AtSimple(new PhWrite(this, "object")));
    }

    @Override
    public Phi lambda() throws Exception {
        return this.take("object");
    }

    /**
     * Attribute that stores object.
     * @since 0.36.0
     */
    private static class AtEncaged implements Attr {
        /**
         * The term to show when empty.
         */
        public static final String EMPTY_TERM = "Ø";

        /**
         * Object in storage.
         */
        private Long locator;

        /**
         * Form of the stored object.
         */
        private String forma;

        /**
         * Ctor.
         */
        AtEncaged() {
            this(null, null);
        }

        /**
         * Ctor for copying.
         * @param locator Locator of object in memory
         * @param form The form of the object
         */
        AtEncaged(final Long locator, final String form) {
            this.locator = locator;
            this.forma = form;
        }

        @Override
        public Attr copy(final Phi self) {
            return new EOcage$EOalloc.AtEncaged(this.locator, this.forma);
        }

        @Override
        public Phi get() {
            if (this.locator == null || !EOcage$EOalloc.OBJECTS.containsKey(this.locator)) {
                throw new ExFailure(
                    "There's no object in storage, can't read"
                );
            }
            return EOcage$EOalloc.OBJECTS.get(this.locator);
        }

        @Override
        public void put(final Phi phi) {
            if (this.forma == null) {
                this.forma = phi.forma();
            } else if (!this.forma.equals(phi.forma())) {
                throw new ExFailure(
                    "Can't write an object formed by %s because object formed by %s was saved before",
                    phi.forma(),
                    this.forma
                );
            }
            if (this.locator == null) {
                synchronized (EOcage$EOalloc.LOCATOR) {
                    this.locator = EOcage$EOalloc.LOCATOR.incrementAndGet();
                }
            }
            EOcage$EOalloc.OBJECTS.put(this.locator, phi);
        }

        @Override
        public String φTerm() {
            final String txt;
            if (this.locator == null || !EOcage$EOalloc.OBJECTS.containsKey(this.locator)) {
                txt = AtEncaged.EMPTY_TERM;
            } else {
                txt = EOcage$EOalloc.OBJECTS.get(this.locator).φTerm();
            }
            return txt;
        }

        @Override
        public String toString() {
            final String txt;
            if (this.locator == null || !EOcage$EOalloc.OBJECTS.containsKey(this.locator)) {
                txt = AtEncaged.EMPTY_TERM;
            } else {
                txt = EOcage$EOalloc.OBJECTS.get(this.locator).toString();
            }
            return txt;
        }
    }
}
