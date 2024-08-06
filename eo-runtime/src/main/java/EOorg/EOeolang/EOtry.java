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
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.util.function.Consumer;
import java.util.function.Function;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * TRY.
 *
 * @since 0.19
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "try")
public final class EOtry extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOtry() {
        this.add("main", new AtVoid("main"));
        this.add("catch", new AtVoid("catch"));
        this.add("finally", new AtVoid("finally"));
    }

    @Override
    public Phi lambda() {
        return new PhTry(this.take("main"), this.take("catch"), this.take("finally"));
    }

    /**
     * Object that knows how to deal with {@link EOorg.EOeolang.EOerror.ExError}.
     * @since 0.36.0
     */
    private static class PhTry implements Phi {
        /**
         * Body object.
         */
        private final Phi body;

        /**
         * Catch object.
         */
        private final Phi ctch;

        /**
         * Finally object.
         */
        private final Phi last;

        /**
         * Put function.
         */
        private final Consumer<Consumer<Phi>> func;

        /**
         * Ctor.
         * @param body Body object
         * @param ctch Catch object
         * @param last Finally object
         */
        PhTry(final Phi body, final Phi ctch, final Phi last) {
            this.body = body;
            this.ctch = ctch;
            this.last = last;
            this.func = new TryExecute(body, ctch);
        }

        @Override
        public void attach(final byte[] data) {
            this.func.accept(phi -> phi.attach(data));
        }

        @Override
        public byte[] delta() {
            return new TryReturn<byte[]>(
                this.body, this.ctch, this.last
            ).apply(Data::delta);
        }

        @Override
        public Phi copy() {
            return new PhTry(this.body.copy(), this.ctch.copy(), this.last.copy());
        }

        @Override
        public Phi take(final String name) {
            return new TryReturn<Phi>(
                this.body, this.ctch, this.last
            ).apply(phi -> phi.take(name));
        }

        @Override
        public boolean put(final int pos, final Phi object) {
            return new TryReturn<Boolean>(
                this.body, this.ctch, this.last
            ).apply(phi -> phi.put(pos, object));
        }

        @Override
        public boolean put(final String name, final Phi object) {
            return new TryReturn<Boolean>(
                this.body, this.ctch, this.last
            ).apply(phi -> phi.put(name, object));
        }

        @Override
        public String locator() {
            return new TryReturn<String>(
                this.body, this.ctch, this.last
            ).apply(Phi::locator);
        }

        @Override
        public String forma() {
            return new TryReturn<String>(
                this.body, this.ctch, this.last
            ).apply(Phi::forma);
        }

        @Override
        public String φTerm() {
            return new TryReturn<String>(
                this.body, this.ctch, this.last
            ).apply(Phi::φTerm);
        }
    }

    /**
     * Tries to execute given function and catches {@link EOorg.EOeolang.EOerror.ExError}.
     * @since 0.36.0
     */
    private static class TryExecute implements Consumer<Consumer<Phi>> {
        /**
         * Body object.
         */
        private final Phi body;

        /**
         * Catch object.
         */
        private final Phi ctch;

        /**
         * Ctor.
         * @param main Body object
         * @param ctch Catch object
         */
        TryExecute(final Phi main, final Phi ctch) {
            this.body = main;
            this.ctch = ctch;
        }

        @Override
        public void accept(final Consumer<Phi> func) {
            try {
                func.accept(this.body);
            } catch (final EOerror.ExError ex) {
                final Phi caught = this.ctch.copy();
                caught.put(0, ex.enclosure());
                func.accept(caught);
            }
        }
    }

    /**
     * Tries to return value from given function and catches {@link EOorg.EOeolang.EOerror.ExError}.
     * @param <T> Type of return value.
     * @since 0.36.0
     */
    private static class TryReturn<T> implements Function<Function<Phi, T>, T> {
        /**
         * Body object.
         */
        private final Phi body;

        /**
         * Catch object.
         */
        private final Phi ctch;

        /**
         * Finally object.
         */
        private final Phi last;

        /**
         * Ctor.
         * @param main Body object
         * @param ctch Catch object
         * @param last Finally object
         */
        TryReturn(final Phi main, final Phi ctch, final Phi last) {
            this.body = main;
            this.ctch = ctch;
            this.last = last;
        }

        @Override
        public T apply(final Function<Phi, T> func) {
            T result;
            try {
                result = func.apply(this.body);
            } catch (final EOerror.ExError ex) {
                final Phi caught = this.ctch.copy();
                caught.put(0, ex.enclosure());
                result = func.apply(caught);
            } finally {
                new Dataized(this.last).take();
            }
            return result;
        }
    }
}
