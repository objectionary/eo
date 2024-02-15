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
import java.util.function.Supplier;
import java.util.Set;
import org.eolang.AtAtom;
import org.eolang.AtCage;
import org.eolang.AtFree;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.Volatile;
import org.eolang.XmirObject;

/**
 * CAGE.
 *
 * @since 0.17
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@Volatile
@XmirObject(oname = "cage")
public final class EOcage extends PhDefault implements Atom {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOcage(final Phi sigma) {
        super(sigma);
        this.add("enclosure", new AtCage());
        this.add("write", new AtWrite(this));
    }

    @Override
    public Phi lambda() {
        return new PhTracedEnclosure(this.attr("enclosure").get(), this.hashCode());
    }

    /**
     * Cage.write attribute.
     * @since 0.33.0
     */
    private static final class AtWrite extends AtAtom {

        /**
         * Ctor.
         * @param cage The {@link EOcage} object
         */
        AtWrite(final Phi cage) {
            super(new EOcage.Write(cage));
        }

        @Override
        public Attr copy(final Phi self) {
            return new AtWrite(self);
        }
    }

    /**
     * Cage write.
     * @since 0.17
     */
    @XmirObject(oname = "cage.write")
    private static final class Write extends PhDefault implements Atom {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Write(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
        }

        @Override
        public Phi lambda() {
            this.attr("σ").get().attr("enclosure").put(
                this.attr("x").get()
            );
            return new Data.ToPhi(true);
        }
    }

    /**
     * Class to trace if the cage got into recursion during the dataization.
     * @since 0.36
     */
    private static final class PhTracedEnclosure implements Phi {

        /**
         * Cages that are currently dataizing. If one cage is datazing and
         * it needs to be dataized inside current dataizing, the cage will be here,
         */
        private static final Set<Integer> DATAIZING_CAGES =
            new ConcurrentHashMap<Integer, Object>().keySet();

        /**
         * Enclosure.
         */
        final Phi enclosure;

        /**
         * Vertex of cage where the {@link PhTracedEnclosure#enclosure}
         * was retrieved.
         */
        final int cage;

        /**
         * Ctor.
         * @param enclosure Enclosure.
         * @param cage Vertex of source cage.
         */
        PhTracedEnclosure(final Phi enclosure, final int cage) {
            this.enclosure = enclosure;
            this.cage = cage;
        }

        @Override
        public Phi copy() {
            return new PhTracedEnclosure(this.enclosure, cage);
        }

        @Override
        public Attr attr(int pos) {
            return this.getAttrSafely(() -> enclosure.attr(pos));
        }

        @Override
        public Attr attr(String name) {
            return this.getAttrSafely(() -> enclosure.attr(name));
        }

        @Override
        public String locator() {
            return enclosure.locator();
        }

        @Override
        public String forma() {
            return enclosure.forma();
        }

        @Override
        public String φTerm() {
            return enclosure.φTerm();
        }

        @Override
        public int hashCode() {
            return enclosure.hashCode();
        }

        /**
         * Get attribute tracing cages.
         * @param supplier Ordinary way to get attribute.
         * @return The {@link Attr} if there is no StackOverflow case.
         */
        private Attr getAttrSafely(final Supplier<Attr> supplier) {
            if (DATAIZING_CAGES.contains(cage)) {
                throw new ExFailure("The cage %s is already dataizing", this.cage);
            }
            DATAIZING_CAGES.add(cage);
            final Attr ret = supplier.get();
            DATAIZING_CAGES.remove(cage);
            return ret;
        }
    }
}
