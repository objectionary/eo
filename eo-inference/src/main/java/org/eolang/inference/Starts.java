/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import org.xembly.Directives;

/**
 * The head every chain starts from, and what needs no working out.
 *
 * <p>Every chain of names is split into reads before the heads are found, so
 * every chain starts from a bare head: a bare {@code Φ} is a copy of {@code Φ},
 * the global object, and a bare {@code ξ} is a copy of the nearest formation
 * around it, found by the nesting of locators, with nothing worked out. In
 * the program</p>
 *
 * <pre> [x] &gt; inc
 *   x.next.foo &gt; @</pre>
 *
 * <p>the only head is the bare {@code ξ} under {@code x.next.foo}, and it
 * gives one row: {@code Φ.inc.φ.ρ.ρ.ρ} is a copy of {@code Φ.inc}. The reads
 * {@code .x}, {@code .next} and {@code .foo} are in the {@code reads}
 * section and get no row here.</p>
 *
 * <p>Two more kinds of row need no reference to look at, and neither is a
 * copy of anything: a datum, the bytes of a literal, and a termination,
 * which comes back with no value at all.</p>
 *
 * <p>Every row here is stated by the text, so the rows are facts and go into
 * the {@code heads} section of {@code facts.xml}, in the order they are
 * found. {@code Resolved} copies them into {@code links.xml}, because the
 * compiler reads them there.</p>
 *
 * @since 0.68.0
 */
final class Starts implements Section {

    @Override
    public Directives directives(final Path xmirs) throws IOException {
        final Xmirs world = new Xmirs(xmirs);
        final Scope scope = new Scope(world.owners());
        final Map<String, Type> found = new LinkedHashMap<>(0);
        for (final XML reference : world.references()) {
            final Noted ref = new Noted(reference);
            final String from = ref.says("loc");
            found.put(from, new Ref(scope.target(from, ref.says("base"))));
        }
        for (final XML datum : world.data()) {
            found.put(new Noted(datum).says("loc"), new Data());
        }
        for (final XML dead : world.terminators()) {
            found.put(new Noted(dead).says("loc"), new Terminator());
        }
        return new Types(found, "heads").directives();
    }
}
