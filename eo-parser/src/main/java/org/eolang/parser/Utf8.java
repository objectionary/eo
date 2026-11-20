/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.bytes.BytesOf;

/**
 * The source text, which has to be valid UTF-8.
 *
 * <p>{@link org.cactoos.text.TextOf} reads bytes through
 * {@code new String(bytes, UTF_8)}, which replaces every malformed sequence
 * with U+FFFD instead of complaining. A file that is not UTF-8 then parses as
 * a different program, and an error points at a character the author never
 * wrote. Here the decoder reports the first malformed byte, and its line is
 * counted in the bytes, before anything is decoded.</p>
 *
 * @since 0.75.0
 */
public final class Utf8 implements Text {

    /**
     * The source to read.
     */
    private final Input input;

    /**
     * The name of the source, if it has one.
     */
    private final String source;

    /**
     * Ctor.
     *
     * @param input The source to read the text from
     */
    public Utf8(final Input input) {
        this(input, null);
    }

    /**
     * Ctor.
     *
     * @param input The source to read the text from
     * @param source The name of the source to report errors against
     */
    public Utf8(final Input input, final String source) {
        this.input = input;
        this.source = source;
    }

    @Override
    public String asString() throws Exception {
        final byte[] bytes = new BytesOf(this.input).asBytes();
        final ByteBuffer data = ByteBuffer.wrap(bytes);
        final CharBuffer chars = CharBuffer.allocate(bytes.length);
        final CharsetDecoder decoder = StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT);
        final CoderResult result = decoder.decode(data, chars, true);
        if (result.isError()) {
            throw new IllegalStateException(this.message(Utf8.line(bytes, data.position())));
        }
        decoder.flush(chars);
        chars.flip();
        return chars.toString();
    }

    private String message(final int line) {
        final String message;
        if (this.source == null) {
            message = String.format("line %d: not valid UTF-8", line);
        } else {
            message = String.format("%s:%d: not valid UTF-8", this.source, line);
        }
        return message;
    }

    private static int line(final byte[] bytes, final int end) {
        int line = 1;
        for (int idx = 0; idx < end; ++idx) {
            if (bytes[idx] == '\n') {
                line = line + 1;
            }
        }
        return line;
    }
}
