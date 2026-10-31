/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * UTF-8 bytes rendered as an EO string literal.
 *
 * <p>The text lands between double quotes, with every glyph spelled by
 * {@link Escaped}. Without the escaping, a quote inside the text closes
 * the literal early and a line feed splits it across lines, so the
 * printed φ-term denotes a string other than the one it came from.</p>
 *
 * <p>Decoding is strict: bytes that are not valid UTF-8 would otherwise
 * turn into replacement characters (U+FFFD), and a term built from
 * those denotes a different object than the one it came from. The
 * caller gets an empty result for such bytes and is expected to print
 * the structural form instead.</p>
 *
 * @since 0.73.3
 */
final class Quoted implements Supplier<Optional<String>> {

    /**
     * The bytes of the text.
     */
    private final byte[] data;

    /**
     * Ctor.
     *
     * @param data The bytes
     */
    Quoted(final byte[] data) {
        this.data = Arrays.copyOf(data, data.length);
    }

    @Override
    public Optional<String> get() {
        Optional<String> result;
        try {
            result = Optional.of(this.quoted());
        } catch (final CharacterCodingException ex) {
            result = Optional.empty();
        }
        return result;
    }

    private String quoted() throws CharacterCodingException {
        final String text = StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .decode(ByteBuffer.wrap(this.data))
            .toString();
        final StringBuilder out = new StringBuilder("\"");
        for (int idx = 0; idx < text.length(); idx = idx + 1) {
            out.append(new Escaped(text.charAt(idx)).get());
        }
        return out.append('"').toString();
    }
}
