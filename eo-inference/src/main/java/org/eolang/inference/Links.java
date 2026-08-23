/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Which types are copies of which.
 *
 * <p>{@link Provides} says what an object has and {@link Needs} says what
 * somebody wants from it, but the two never meet: the void {@code x} of a
 * formation and the argument put into it are different objects with
 * different types, and only a reference says they are the same thing. That
 * is this clue. In the program of the design note</p>
 *
 * <pre> [] &gt; app
 *   inc t &gt; @
 *   [] &gt; t
 *     [] &gt; next
 *   [x] &gt; inc
 *     x.next.foo &gt; @</pre>
 *
 * <p>three references give three rows: {@code Φ.app.φ} is a copy of
 * {@code Φ.app.inc}, {@code Φ.app.φ.α0} is a copy of {@code Φ.app.t}, and
 * {@code Φ.app.inc.φ.ρ.ρ} is a copy of {@code Φ.app.inc.x}. Follow them and
 * the two tables finally touch: what {@code x} is asked for is what
 * {@code t} must have.</p>
 *
 * <p>For now "a copy of" means "the same as" — whatever one of them has or
 * needs, the other one does too. The rows are kept instead of renaming one
 * locator into the other in the XMIR on purpose: this is the one place
 * where the checker later gets smarter, when a copy starts receiving types
 * of its own, and then nothing else has to change.</p>
 *
 * <p>Which is why a row carries what an object is as an element of its own
 * rather than as a cell, and what {@link Types} makes of it. Being a copy is
 * one of the things an object turns out to be, and three more arrive here as
 * well, none of them a copy of anything: a datum, the bytes of a literal being
 * the ground the program stands on; a termination, which comes back with no
 * value at all; and a void, which comes back with whatever a caller puts in
 * it. Those are the rows this clue writes without a reference to look at.</p>
 *
 * <p>A name that resolves to nothing gets no row and no complaint: a
 * missing row makes a later check stay undecided, while a wrong row would
 * make it decide wrongly. On the runtime this happens to 730 references out
 * of 21,555, and every one of them is {@code ξ.ρ} — the object one step
 * out, which no formation binds as an attribute and which therefore cannot
 * be found by looking for a name. Linking it needs the notion of "the
 * object I am inside of", which the checking loop will have anyway.</p>
 *
 * @since 0.68.0
 */
final class Links implements Clue {

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        final Xmirs world = new Xmirs(xmirs);
        final Collection<String> made = new ArrayList<>(0);
        for (final XML formation : world.formations()) {
            made.add(formation.xpath("@loc").get(0));
        }
        final Scope scope = new Scope(new HashSet<>(world.locators()), new HashSet<>(made));
        final Map<String, Type> found = new LinkedHashMap<>(0);
        for (final XML reference : world.references()) {
            final String from = reference.xpath("@loc").get(0);
            final String target = scope.target(from, reference.xpath("@base").get(0));
            if (!target.isEmpty()) {
                found.put(from, new Ref(target));
            }
        }
        for (final XML datum : world.data()) {
            found.put(datum.xpath("@loc").get(0), new Data());
        }
        for (final XML dead : world.terminators()) {
            found.put(dead.xpath("@loc").get(0), new Terminator());
        }
        for (final XML hollow : world.voids()) {
            found.put(hollow.xpath("@loc").get(0), new Var());
        }
        Files.createDirectories(tables);
        Files.write(
            tables.resolve("links.xml"),
            new Types(found).asXml().toString().getBytes(StandardCharsets.UTF_8)
        );
    }
}
