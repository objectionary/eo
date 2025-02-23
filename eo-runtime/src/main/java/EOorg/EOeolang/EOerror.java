/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
         * Messages seen on its way out.
         */
        private final Collection<String> trace;

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
         * @param message New message
         */
        public ExError(final Phi cause, final String message) {
            this(cause, Collections.singletonList(message));
        }

        /**
         * Ctor.
         * @param cause Previous error
         * @param message New message
         */
        public ExError(final ExError cause, final String message) {
            this(cause.enclosure(), concat(cause.trace, message));
        }

        /**
         * Ctor.
         * @param enclosure Enclosure inside the error
         * @param before Messages seen before
         */
        public ExError(final Phi enclosure, final Collection<String> before) {
            super(EOerror.ExError.safeMessage(enclosure));
            this.enc = enclosure;
            this.trace = before;
        }

        @Override
        public String toString() {
            return String.format(
                "%s +%s",
                super.toString(),
                this.trace.size()
            );
        }

        /**
         * Take it.
         * @return The enclosed object
         */
        public Phi enclosure() {
            return this.enc;
        }

        /**
         * Take earlier seen messages.
         * @return The messages
         */
        public Collection<String> messages() {
            return this.trace;
        }

        /**
         * Concatenate messages.
         * @param before Locations before
         * @param message New one
         * @return New list of them
         */
        private static Collection<String> concat(final Collection<String> before,
            final String message) {
            final Collection<String> list = new ArrayList<>(before.size() + 1);
            list.addAll(before);
            list.add(message);
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
