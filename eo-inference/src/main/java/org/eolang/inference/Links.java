/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjCached;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

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
        final Tojos rows = new TjCached(new TjDefault(new MnMemory()));
        int seen = 0;
        for (final XML reference : world.references()) {
            final String from = reference.xpath("@loc").get(0);
            final String target = scope.target(from, reference.xpath("@base").get(0));
            if (!target.isEmpty()) {
                rows.add(from)
                    .set("index", Integer.toString(seen))
                    .set("copy", target);
                seen = seen + 1;
            }
        }
        Files.createDirectories(tables);
        Files.write(
            tables.resolve("links.xml"),
            new Grouped(rows, "links").asXml().toString().getBytes(StandardCharsets.UTF_8)
        );
    }
}
