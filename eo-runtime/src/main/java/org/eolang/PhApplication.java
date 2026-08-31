/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A attr-putting object.
 * @since 0.1
 */
public final class PhApplication extends PhOnce {

    /**
     * Matcher of raw bytes inside a φ-term, like {@code [D> 40-45-00]}.
     */
    private static final Pattern DATA = Pattern.compile("\\[D> ([0-9A-F-]*)]");

    /**
     * The dash that terminates a single-byte hex string.
     */
    private static final Pattern TRAILING = Pattern.compile("-$");

    /**
     * Ctor.
     * @param phi The object
     * @param binds The bindings to apply, in order
     */
    public PhApplication(final Phi phi, final Bind... binds) {
        this(
            () -> {
                final Phi copy = phi.copy();
                for (final Bind bind : binds) {
                    bind.attach(copy);
                }
                return copy;
            },
            () -> PhApplication.applied(phi, binds)
        );
    }

    /**
     * Ctor.
     * @param phi The object
     * @param pos The position
     * @param attr The value
     */
    public PhApplication(final Phi phi, final int pos, final Phi attr) {
        this(phi, new Bind(pos, attr));
    }

    /**
     * Ctor.
     * @param phi The object
     * @param name The name of attr
     * @param attr The value
     */
    public PhApplication(final Phi phi, final String name, final Phi attr) {
        this(phi, new Bind(name, attr));
    }

    /**
     * Ctor.
     * @param sup Supplier of the wrapped object
     * @param term Supplier of the φ-term
     */
    private PhApplication(final Supplier<Phi> sup, final Supplier<String> term) {
        super(sup, term);
    }

    private static String applied(final Phi phi, final Bind... binds) {
        final String head = phi.φTerm();
        final String body = PhApplication.body(binds);
        final Matcher data = PhApplication.DATA.matcher(body);
        final boolean literal = binds.length == 1 && binds[0].first() && data.find();
        final String string;
        if (literal && "Φ.string".equals(head)) {
            string = PhApplication.string(PhApplication.bytes(data.group(1)));
        } else {
            string = null;
        }
        final String result;
        if (string != null) {
            result = string;
        } else if (literal && "Φ.number".equals(head)) {
            result = new Numeral(
                new BytesOf(PhApplication.bytes(data.group(1))).asNumber()
            ).get();
        } else {
            result = String.format("%s(%s)", head, body);
        }
        return result;
    }

    private static String string(final byte[] bytes) {
        return new Quoted(bytes).get().orElse(null);
    }

    private static String body(final Bind... binds) {
        final StringBuilder out = new StringBuilder();
        for (int idx = 0; idx < binds.length; ++idx) {
            if (idx > 0) {
                out.append(',');
            }
            out.append(binds[idx].φTerm());
        }
        return out.toString();
    }

    private static byte[] bytes(final String hex) {
        final byte[] bytes;
        if (hex.isEmpty() || "--".equals(hex)) {
            bytes = new byte[0];
        } else {
            final String[] parts = PhApplication.TRAILING
                .matcher(hex)
                .replaceAll("")
                .split("-", -1);
            bytes = new byte[parts.length];
            for (int idx = 0; idx < parts.length; ++idx) {
                bytes[idx] = (byte) Integer.parseInt(parts[idx], 16);
            }
        }
        return bytes;
    }
}
