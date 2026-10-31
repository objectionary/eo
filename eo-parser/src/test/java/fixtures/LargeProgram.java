/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package fixtures;

import java.io.InputStream;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

/**
 * EO program of as many nested objects as the caller asks for.
 *
 * @since 0.61.0
 */
public final class LargeProgram implements Input {

    /**
     * How many objects the program declares.
     */
    private final int count;

    /**
     * Constructor.
     *
     * @param objects How many objects the program declares
     */
    public LargeProgram(final int objects) {
        this.count = objects;
    }

    @Override
    public InputStream stream() throws Exception {
        return new InputOf(
            Stream.concat(
                Stream.concat(
                    Stream.of("[] > large"),
                    IntStream.range(0, this.count).boxed().flatMap(
                        idx -> Stream.of(
                            String.format("  [n] > fibo%d", idx),
                            "    n.plus 1 > @"
                        )
                    )
                ),
                Stream.of("")
            ).collect(Collectors.joining(System.lineSeparator()))
        ).stream();
    }
}
