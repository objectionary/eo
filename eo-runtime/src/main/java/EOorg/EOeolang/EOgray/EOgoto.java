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

package EOorg.EOeolang.EOgray;

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * GOTO.
 *
 * @since 0.17
 */
@XmirObject(oname = "goto")
public class EOgoto extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOgoto(final Phi sigma) {
        super(sigma);
        this.add("f", new AtFree());
        this.add("φ", new AtComposite(this, rho -> {
            final Phi body = rho.attr("f").get().copy(rho);
            body.attr(0).put(new EOgoto.Token(rho));
            Phi ret;
            while (true) {
                try {
                    ret = new Data.ToPhi(new Dataized(body).take());
                    break;
                } catch (final EOgoto.BackwardException ex) {
                    ret = new Data.ToPhi(true);
                } catch (final EOgoto.ForwardException ex) {
                    ret = ex.ret;
                    break;
                }
            }
            return ret;
        }));
    }

    /**
     * The token.
     * @since 0.17
     */
    @XmirObject(oname = "goto.token")
    private final class Token extends PhDefault {
        Token(final Phi sigma) {
            super(sigma);
            this.add("backward", new AtComposite(this, EOgoto.Backward::new));
            this.add("forward", new AtComposite(this, EOgoto.Forward::new));
        }
    }

    /**
     * Backward.
     * @since 0.17
     */
    @XmirObject(oname = "goto.token.backward")
    private final class Backward extends PhDefault {
        Backward(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(this, rho -> {
                throw new EOgoto.BackwardException();
            }));
        }
    }

    /**
     * Forward.
     * @since 0.17
     */
    @XmirObject(oname = "goto.token.forward")
    private final class Forward extends PhDefault {
        Forward(final Phi sigma) {
            super(sigma);
            this.add("ret", new AtFree());
            this.add("φ", new AtComposite(this, rho -> {
                throw new EOgoto.ForwardException(rho.attr("ret").get());
            }));
        }
    }

    /**
     * When going back.
     * @since 0.17
     */
    private static class BackwardException extends Attr.FlowException {
        private static final long serialVersionUID = 1735493012609760997L;
    }

    /**
     * When going forward.
     * @since 0.17
     */
    private static class ForwardException extends Attr.FlowException {
        private static final long serialVersionUID = 1501718836588849754L;
        final Phi ret;
        ForwardException(final Phi phi) {
            this.ret = phi;
        }
    }

}
