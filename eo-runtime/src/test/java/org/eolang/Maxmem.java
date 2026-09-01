/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.lang.reflect.Method;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.InvocationInterceptor;
import org.junit.jupiter.api.extension.ReflectiveInvocationContext;

/**
 * A limit on the memory a single test may take.
 *
 * <p>A test that allocates more than the {@code eo.maxmem} bytes granted to
 * it is terminated and reported as skipped, in the same way
 * {@link Deadline} deals with a test that outlives {@code eo.deadline}.
 * Without it, one greedy test takes the whole heap down with it and the
 * build ends with {@code OutOfMemoryError} in a test that did nothing
 * wrong, on a line that says nothing about who ate the memory.</p>
 *
 * <p>The limit is the number of bytes the test allocates, counted the only
 * way a shared heap allows it to be counted per test - see
 * {@link Consumed}. It is read from the {@code eo.maxmem} system property,
 * which understands the suffixes of {@code -Xmx}: {@code 1G}, {@code 512M},
 * {@code 65536K}, or plain bytes. An empty property, or a zero, means no
 * limit at all and no watching either.</p>
 *
 * @since 0.75.0
 */
public final class Maxmem implements InvocationInterceptor {

    /**
     * What a valid value of the property looks like.
     */
    private static final Pattern SIZE = Pattern.compile("(\\d+)\\s*([kKmMgG]?)[bB]?");

    /**
     * How many bytes a single test may allocate, zero if there is no limit.
     */
    private static final long LIMIT = Maxmem.limit(System.getProperty("eo.maxmem"));

    /**
     * Ctor.
     */
    public Maxmem() {
        // nothing
    }

    @Override
    public void interceptTestMethod(final Invocation<Void> invocation,
        final ReflectiveInvocationContext<Method> context,
        final ExtensionContext extension) throws Throwable {
        new Watched(Maxmem.LIMIT).through(invocation);
    }

    @Override
    public void interceptTestTemplateMethod(final Invocation<Void> invocation,
        final ReflectiveInvocationContext<Method> context,
        final ExtensionContext extension) throws Throwable {
        new Watched(Maxmem.LIMIT).through(invocation);
    }

    /**
     * How many bytes the property stands for.
     * @param text What the property says, may be NULL or empty
     * @return Bytes a test may allocate, zero if there is no limit
     */
    static long limit(final String text) {
        final String trimmed;
        if (text == null) {
            trimmed = "";
        } else {
            trimmed = text.trim();
        }
        final long bytes;
        if (trimmed.isEmpty()) {
            bytes = 0L;
        } else {
            final Matcher matcher = Maxmem.SIZE.matcher(trimmed);
            if (!matcher.matches()) {
                throw new IllegalArgumentException(
                    String.format(
                        "Can't understand '%s' as the value of eo.maxmem, try '1G' or '512M'",
                        trimmed
                    )
                );
            }
            bytes = Long.parseLong(matcher.group(1)) * Maxmem.scale(matcher.group(2));
        }
        return bytes;
    }

    private static long scale(final String suffix) {
        long scale = 1L;
        if (!suffix.isEmpty()) {
            final int power = 1 + "kmg".indexOf(
                Character.toLowerCase(suffix.charAt(0))
            );
            for (int idx = 0; idx < power; ++idx) {
                scale *= 1024L;
            }
        }
        return scale;
    }
}
