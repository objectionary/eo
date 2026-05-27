/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.nio.charset.StandardCharsets;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A attr-putting object.
 * @since 0.1
 */
public final class PhWith extends PhOnce {

    /**
     * Matcher of raw bytes inside a φ-term, like {@code [D> 40-45-00]}.
     */
    private static final Pattern DATA = Pattern.compile("\\[D> ([0-9A-F-]*)]");

    /**
     * Ctor.
     * @param phi The object
     * @param pos The position
     * @param attr The value
     */
    public PhWith(final Phi phi, final int pos, final Phi attr) {
        this(
            () -> {
                phi.put(pos, attr);
                return phi;
            },
            () -> PhWith.applied(phi, pos, attr)
        );
    }

    /**
     * Ctor.
     * @param phi The object
     * @param name The name of attr
     * @param attr The value
     */
    public PhWith(final Phi phi, final String name, final Phi attr) {
        this(
            () -> {
                phi.put(name, attr);
                return phi;
            },
            () -> String.format("%s(%s->%s)", phi.φTerm(), name, attr.φTerm())
        );
    }

    /**
     * Ctor.
     * @param sup Supplier of the wrapped object
     * @param term Supplier of the φ-term
     */
    private PhWith(final Supplier<Phi> sup, final Supplier<String> term) {
        super(sup, term);
    }

    /**
     * The φ-term of a positional application, rendering a literal number or
     * string construction as its value.
     * @param phi The receiver
     * @param pos The position
     * @param attr The bound value
     * @return The φ-term
     */
    private static String applied(final Phi phi, final int pos, final Phi attr) {
        final String head = phi.φTerm();
        final String inner = attr.φTerm();
        final Matcher data = PhWith.DATA.matcher(inner);
        final boolean literal = pos == 0 && data.find();
        final String result;
        if (literal && "Φ.string".equals(head)) {
            result = String.format(
                "\"%s\"", new String(PhWith.bytes(data.group(1)), StandardCharsets.UTF_8)
            );
        } else if (literal && "Φ.number".equals(head)) {
            result = PhDefault.numeral(new BytesOf(PhWith.bytes(data.group(1))).asNumber());
        } else {
            result = String.format("%s(%d->%s)", head, pos, inner);
        }
        return result;
    }

    /**
     * Parse a dash-separated hex string into bytes.
     * @param hex The hex, like "40-45-00"
     * @return The byte array
     */
    private static byte[] bytes(final String hex) {
        final byte[] bytes;
        if (hex.isEmpty()) {
            bytes = new byte[0];
        } else {
            final String[] parts = hex.split("-");
            bytes = new byte[parts.length];
            for (int idx = 0; idx < parts.length; ++idx) {
                bytes[idx] = (byte) Integer.parseInt(parts[idx], 16);
            }
        }
        return bytes;
    }
}
