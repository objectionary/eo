/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_org.EO_eolang; // NOPMD

import java.util.concurrent.atomic.AtomicInteger;
import org.eolang.PhDefault;
import org.eolang.XmirObject;

/**
 * Fixture object that counts how often it is built.
 *
 * <p>A package builds its member by name, through reflection, so the count
 * cannot live anywhere but in a static: there is no instance to ask before
 * the instance exists. It says whether a first take under many threads builds
 * the member once or once per thread (#7700).</p>
 *
 * @since 0.74.0
 */
@XmirObject(oname = "probe")
public final class EOprobe extends PhDefault {

    /**
     * How many of these were built.
     */
    public static final AtomicInteger BUILT = new AtomicInteger(0);

    /**
     * Ctor.
     *
     * @checkstyle ConstructorsCodeFreeCheck (12 lines)
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOprobe() {
        super();
        EOprobe.BUILT.incrementAndGet();
        try {
            Thread.sleep(100L);
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
        }
    }
}
