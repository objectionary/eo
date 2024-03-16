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

import org.eolang.AtAtom;
import org.eolang.AtFree;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * GOTO.
 *
 * @since 0.17
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "goto")
public final class EOgoto extends PhDefault implements Atom {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOgoto(final Phi sigma) {
        super(sigma);
        this.add("f", new AtFree());
    }

    @Override
    public Phi lambda() {
        final Phi body = this.attr("f").get().copy();
        body.attr("ρ").put(this);
        body.attr(0).put(new EOgoto.Token(this));
        Phi ret;
        while (true) {
            try {
                ret = body;
                new Dataized(body).take();
                break;
            } catch (final EOgoto.BackwardException ex) {
                if (!ex.sigma.attr("σ").get().equals(this)) {
                    throw ex;
                }
            } catch (final EOgoto.ForwardException ex) {
                if (!ex.sigma.attr("σ").get().equals(this)) {
                    throw ex;
                }
                ret = ex.ret;
                break;
            }
        }
        return ret;
    }

    /**
     * The token.
     * @since 0.17
     */
    @XmirObject(oname = "goto.token")
    private static final class Token extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Token(final Phi sigma) {
            super(sigma);
            this.add("backward", new EOgoto.Token.AtBackward(this));
            this.add("forward", new EOgoto.Token.AtForward(this));
        }

        /**
         * Goto.token.forward attribute.
         * @since 0.33.0
         */
        private static final class AtForward extends AtAtom {

            /**
             * Ctor.
             * @param token The {@link EOgoto.Token} object
             */
            AtForward(final Phi token) {
                super(new EOgoto.Forward(token));
            }

            @Override
            public Attr copy(final Phi self) {
                return new AtForward(self);
            }
        }

        /**
         * Goto.token.backward attribute.
         * @since 0.33.0
         */
        private static final class AtBackward extends AtAtom {
            /**
             * Ctor.
             * @param token The {@link EOgoto.Token} object
             */
            AtBackward(final Phi token) {
                super(new EOgoto.Backward(token));
            }

            @Override
            public Attr copy(final Phi self) {
                return new AtBackward(self);
            }
        }
    }

    /**
     * Backward.
     * @since 0.17
     */
    @XmirObject(oname = "goto.token.backward")
    private static final class Backward extends PhDefault implements Atom {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Backward(final Phi sigma) {
            super(sigma);
        }

        @Override
        public Phi lambda() {
            throw new EOgoto.BackwardException(
                this.attr("σ").get()
            );
        }
    }

    /**
     * Forward.
     * @since 0.17
     */
    @XmirObject(oname = "goto.token.forward")
    private static final class Forward extends PhDefault implements Atom {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Forward(final Phi sigma) {
            super(sigma);
            this.add("ret", new AtFree());
        }

        @Override
        public Phi lambda() {
            throw new EOgoto.ForwardException(
                this.attr("σ").get(),
                this.attr("ret").get()
            );
        }
    }

    /**
     * When going back.
     * @since 0.17
     */
    private static class BackwardException extends ExAbstract {
        /**
         * Serialization identifier.
         */
        private static final long serialVersionUID = 1735493012609760997L;

        /**
         * Sigma.
         */
        private final Phi sigma;

        /**
         * Ctor.
         * @param sgm Sigma
         */
        BackwardException(final Phi sgm) {
            super();
            this.sigma = sgm;
        }
    }

    /**
     * When going forward.
     * @since 0.17
     */
    private static class ForwardException extends ExAbstract {
        /**
         * Serialization identifier.
         */
        private static final long serialVersionUID = 1501718836588849754L;

        /**
         * Sigma.
         */
        private final Phi sigma;

        /**
         * Return.
         */
        private final Phi ret;

        /**
         * Ctor.
         * @param sgm Sigma
         * @param phi Return
         */
        ForwardException(final Phi sgm, final Phi phi) {
            super();
            this.sigma = sgm;
            this.ret = phi;
        }
    }

}
