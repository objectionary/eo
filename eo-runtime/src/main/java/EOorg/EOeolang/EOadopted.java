/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import org.eolang.Attr;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * ADOPTED.
 *
 * This class is thread-safe.
 *
 * @since 0.29
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "adopted")
public final class EOadopted implements Phi, Cloneable {

    /**
     * The kid.
     */
    private Phi kid;

    /**
     * The parent.
     */
    private Phi parent;

    /**
     * Moved already?
     */
    private final AtomicBoolean moved = new AtomicBoolean();

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOadopted(final Phi sigma) {
        // ignore sigma
    }

    @Override
    public Phi copy() {
        final Phi copy;
        if (this.kid == null) {
            copy = this;
        } else {
            this.moveIt();
            copy = this.kid.copy();
        }
        return copy;
    }

    @Override
    public void move(final Phi rho) {
        // ignore this
    }

    @Override
    public Attr attr(final int pos) {
        final Attr attr;
        if (this.kid == null && pos == 0) {
            attr = new EOadopted.Inject(
                phi -> {
                    synchronized (this.moved) {
                        this.kid = phi;
                    }
                }
            );
        } else if (this.parent == null && pos == 1) {
            attr = new EOadopted.Inject(
                phi -> {
                    synchronized (this.moved) {
                        this.parent = phi;
                    }
                }
            );
        } else {
            this.moveIt();
            attr = this.kid.attr(pos);
        }
        return attr;
    }

    @Override
    public Attr attr(final String name) {
        final Attr attr;
        if (this.kid == null) {
            attr = this.attr(0);
        } else if (this.parent == null) {
            attr = this.attr(1);
        } else {
            this.moveIt();
            attr = this.kid.attr(name);
        }
        return attr;
    }

    @Override
    public String locator() {
        this.moveIt();
        return this.kid.locator();
    }

    @Override
    public String φTerm() {
        this.moveIt();
        return this.kid.φTerm();
    }

    /**
     * Move the kid to the parent, if necessary.
     */
    private void moveIt() {
        synchronized (this.moved) {
            if (this.kid != null && this.parent != null && !this.moved.get()) {
                this.kid.move(this.parent);
                this.moved.set(true);
            }
        }
    }

    /**
     * Injector of the kid or the parent.
     *
     * @since 0.29
     */
    private static final class Inject implements Attr {
        /**
         * The evaluator.
         */
        private final Consumer<Phi> target;

        /**
         * Ctor.
         * @param tgt The evaluator
         */
        Inject(final Consumer<Phi> tgt) {
            this.target = tgt;
        }

        @Override
        public Attr copy(final Phi self) {
            throw new UnsupportedOperationException("#copy()");
        }

        @Override
        public Phi get() {
            throw new UnsupportedOperationException("#get()");
        }

        @Override
        public void put(final Phi phi) {
            this.target.accept(phi);
        }

        @Override
        public String φTerm() {
            throw new UnsupportedOperationException("#φTerm()");
        }
    }
}
