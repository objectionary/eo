/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package fixtures;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Iterator;
import java.util.concurrent.TimeUnit;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

/**
 * EO program handed over only after a pause, so that a test knows how long
 * the reading of it takes at the very least.
 *
 * <p>Every call of {@link #stream()} waits for the next pause of the list
 * given to the constructor, which lets a test tell one reading of the same
 * program from the next one.</p>
 *
 * @since 0.73.4
 */
public final class SlowProgram implements Input {

    /**
     * The pauses, in milliseconds, one for every call.
     */
    private final Iterator<Long> pauses;

    /**
     * Constructor.
     *
     * @param millis How long every call waits before it hands the program over
     */
    public SlowProgram(final Long... millis) {
        this(Arrays.asList(millis).iterator());
    }

    /**
     * Constructor.
     *
     * @param waits The pauses, in milliseconds, one for every call
     */
    private SlowProgram(final Iterator<Long> waits) {
        this.pauses = waits;
    }

    @Override
    public InputStream stream() throws Exception {
        TimeUnit.MILLISECONDS.sleep(this.pauses.next());
        return new InputOf(String.format("# Slow.%n[] > slow%n")).stream();
    }
}
