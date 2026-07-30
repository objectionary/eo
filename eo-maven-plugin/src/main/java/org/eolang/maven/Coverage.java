/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.Train;
import java.io.IOException;
import java.util.Collection;
import java.util.function.Function;

/**
 * EO object coverage instrumentation, as configured for one {@code transpile}
 * run.
 * <p>
 *     There are two implementations: {@link CoverageManifest}, which wraps
 *     every located object into {@code PhCoverage} and records where those
 *     wrappers went, and {@link Coverage.Off}, which does nothing at all.
 *     {@link Transpiling} talks to one of them without ever asking which one
 *     it got.
 * </p>
 * @since 0.62.0
 */
interface Coverage {

    /**
     * Whether every located object in the generated Java is wrapped into
     * {@code PhCoverage}. Both the {@code coverage} parameter of
     * {@code to-java.xsl} and the transpile cache key are derived from it,
     * since it changes what the transpiler emits.
     * @return True if the objects are wrapped
     */
    boolean tracked();

    /**
     * The XMIR that the final {@code to-java.xsl} shift has to be applied
     * to, with one manifest line per location that shift is going to
     * instrument in it added to {@code found}.
     * <p>
     *     {@link CoverageManifest} applies {@code shifts} here, in front of
     *     the caller's transpile cache, because a manifest that skips the
     *     sources the cache answers without transpiling them again would
     *     under-report the number of instrumented lines. {@link Coverage.Off}
     *     applies nothing and hands the XMIR back untouched, so a build
     *     without instrumentation still lets the cache skip the whole train.
     * </p>
     * @param xmir The XMIR to transpile
     * @param shifts Every transpile shift except {@code to-java.xsl}
     * @param found Where to add the manifest lines
     * @return The XMIR to transpile further
     */
    XML located(XML xmir, Function<XML, XML> shifts, Collection<String> found);

    /**
     * The train still to be applied to what {@link #located(XML, Function,
     * Collection)} returned: the whole one when it returned the XMIR
     * untouched, only {@code to-java.xsl} when it already ran everything
     * else.
     * @param whole Every transpile shift, in one train
     * @param last The final {@code to-java.xsl} shift, alone
     * @return The train to apply
     */
    Train<Shift> pending(Train<Shift> whole, Train<Shift> last);

    /**
     * Save every manifest line collected during the run, in one write, once
     * the transpiler's worker pool has drained.
     * @param found The lines, one per instrumented location
     * @throws IOException If fails to write
     */
    void save(Collection<String> found) throws IOException;

    /**
     * Coverage that instruments nothing.
     * @since 0.62.0
     */
    final class Off implements Coverage {

        @Override
        public boolean tracked() {
            return false;
        }

        @Override
        public XML located(
            final XML xmir, final Function<XML, XML> shifts, final Collection<String> found
        ) {
            return xmir;
        }

        @Override
        public Train<Shift> pending(final Train<Shift> whole, final Train<Shift> last) {
            return whole;
        }

        @Override
        public void save(final Collection<String> found) {
            Logger.debug(this, "Coverage is off, no manifest to save");
        }
    }
}
