/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import java.nio.charset.StandardCharsets;
import org.eolang.AtFree;
import org.eolang.AtLambda;
import org.eolang.Attr;
import org.eolang.Bytes;
import org.eolang.BytesOf;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * ERROR.
 *
 * <p>You are NOT supposed to use this object programmatically. It is only
 * used from EO, but not from Java. From Java you just throw
 * {@link ExFailure}. It will be properly caught and converted to the error.
 * Again, DON'T USE THIS OBJECT PROGRAMMATICALLY.
 *
 * @since 0.22
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "error")
public final class EOerror extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOerror(final Phi sigma) {
        super(sigma);
        this.add("α", new AtFree());
        this.add(
            Attr.LAMBDA,
            new AtLambda(
                this,
                rho -> {
                    final Phi enclosure = rho.attr("α").get();
                    enclosure.attr("ρ").put(this);
                    throw new ExError(enclosure);
                }
            )
        );
    }

    /**
     * Make a message from an exception.
     * @param exp The exception
     * @return Message
     */
    public static String message(final Throwable exp) {
        final StringBuilder ret = new StringBuilder(0);
        if (exp.getMessage() != null) {
            if (!(exp instanceof ExFailure)) {
                ret.append(exp.getClass().getSimpleName()).append(": ");
            }
            ret.append(exp.getMessage().replace("%", "%%"));
        }
        if (exp.getCause() != null) {
            ret.append("; caused by ").append(EOerror.message(exp.getCause()));
        }
        return ret.toString();
    }

    /**
     * This exception is thrown by the {@link EOerror} object only.
     *
     * <p>You are not supposed to use it anywhere else!</p>
     *
     * @since 0.24
     */
    public static final class ExError extends ExAbstract {

        /**
         * Serialization identifier.
         */
        private static final long serialVersionUID = 1735493012609760997L;

        /**
         * Enclosure.
         */
        private final Phi enc;

        /**
         * Ctor.
         * @param enclosure Enclosure inside the error
         */
        public ExError(final Phi enclosure) {
            super(EOerror.ExError.safeMessage(enclosure));
            this.enc = enclosure;
        }

        /**
         * Take it.
         * @return The enclosed object
         */
        public Phi enclosure() {
            return this.enc;
        }

        /**
         * Retrieve message from enclosure safely.
         * @param enclosure Enclosure.
         * @return String message.
         * @checkstyle IllegalCatchCheck (45 lines)
         */
        private static String safeMessage(final Phi enclosure) {
            String result;
            if (enclosure == null) {
                result = "null Phi";
            } else {
                try {
                    final byte[] data = new Dataized(enclosure).take();
                    switch (data.length) {
                        case 0:
                            result = String.format(
                                "%s(Δ = --)",
                                enclosure
                            );
                            break;
                        case 1:
                            result = String.format(
                                "%s(Δ = %s)",
                                enclosure,
                                data[0] > 0
                            );
                            break;
                        case 8:
                            final Bytes bytes = new BytesOf(data);
                            result = String.format(
                                "%s(Δ = %s = %s, or %s, or %s)",
                                enclosure,
                                bytes,
                                bytes.asNumber(Long.class),
                                bytes.asNumber(Double.class),
                                new String(data, StandardCharsets.UTF_8)
                            );
                            break;
                        default:
                            result = String.format(
                                "%s(Δ = %s or %s)",
                                enclosure,
                                new BytesOf(data),
                                new String(data, StandardCharsets.UTF_8)
                            );
                    }
                } catch (final Throwable first) {
                    try {
                        result = enclosure.toString();
                    } catch (final Throwable second) {
                        result = enclosure.getClass().toString();
                    }
                }
            }
            return result;
        }
    }
}
