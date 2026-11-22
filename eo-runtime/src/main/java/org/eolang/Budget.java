/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * How much memory a single test may allocate, written on the test itself.
 *
 * <p>The transpiler puts it on every test it makes out of an {@code ++>} or
 * {@code -->} attribute of an {@code .eo} file. {@link Maxmem} reads it while
 * the test runs and stops the test once it has allocated more. A test keeps
 * its budget wherever it is compiled, so a build that configures nothing for
 * its tests still cannot lose its whole heap to one of them.</p>
 *
 * <p>The size is written the way {@code -Xmx} writes it: {@code 1G},
 * {@code 512M}, {@code 65536K}, or plain bytes. A zero, or nothing at all,
 * means no limit. The budget on a test wins over the {@code eo.maxmem}
 * system property, the same way the {@code @Timeout} of a test wins over the
 * default timeout of JUnit.</p>
 *
 * @since 0.64.0
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Budget {

    /**
     * How much memory the test may allocate.
     *
     * @return The size, like {@code 1G}
     */
    String value();
}
