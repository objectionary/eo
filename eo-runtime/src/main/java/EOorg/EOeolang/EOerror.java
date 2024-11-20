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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.VerboseBytesAsString;
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
public final class EOerror extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOerror() {
        this.add("message", new AtVoid("message"));
    }

    @Override
    public Phi lambda() {
        throw new ExError(this.take("message"));
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
         * Locations seen on its way out.
         */
        private final Collection<String> locs;

        /**
         * Ctor.
         * @param enclosure Enclosure inside the error
         */
        public ExError(final Phi enclosure) {
            this(enclosure, Collections.emptyList());
        }

        /**
         * Ctor.
         * @param cause Previous error
         * @param loc New location
         */
        public ExError(final ExError cause, final String loc) {
            this(cause.enclosure(), concat(cause.locs, loc));
        }

        /**
         * Ctor.
         * @param enclosure Enclosure inside the error
         * @param locations Locations seen
         */
        public ExError(final Phi enclosure, final Collection<String> locations) {
            super(EOerror.ExError.safeMessage(enclosure));
            this.enc = enclosure;
            this.locs = locations;
        }

        /**
         * Take it.
         * @return The enclosed object
         */
        public Phi enclosure() {
            return this.enc;
        }

        /**
         * Take locations.
         * @return The locations
         */
        public Collection<String> locations() {
            return this.locs;
        }

        /**
         * Concatenate locations.
         * @param before Locations before
         * @param loc New one
         * @return New list of them
         */
        private static Collection<String> concat(final Collection<String> before,
            final String loc) {
            final Collection<String> list = new ArrayList<>(before.size() + 1);
            list.addAll(before);
            list.add(loc);
            return list;
        }

        /**
         * Retrieve message from enclosure safely.
         * @param enclosure Enclosure.
         * @return String message.
         * @checkstyle IllegalCatchCheck (55 lines)
         */
        @SuppressWarnings("PMD.AvoidCatchingGenericException")
        private static String safeMessage(final Phi enclosure) {
            String result;
            if (enclosure == null) {
                result = "null Phi";
            } else {
                try {
                    final byte[] raw = new Dataized(enclosure).take();
                    result = String.format(
                        "%s(Δ = %s)",
                        enclosure,
                        new VerboseBytesAsString(raw).get()
                    );
                } catch (final Exception first) {
                    try {
                        result = enclosure.toString();
                    } catch (final Exception second) {
                        result = enclosure.getClass().toString();
                    }
                }
            }
            return result;
        }
    }
}
