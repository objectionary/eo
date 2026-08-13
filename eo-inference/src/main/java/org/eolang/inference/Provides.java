/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjDeferred;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;

/**
 * What every object certainly has.
 *
 * <p>This is the first and the simplest rule of the checker: a formation
 * shows its attributes right in the code, so we can write them down
 * without thinking. This formation</p>
 *
 * <pre> [] &gt; t
 *   [] &gt; next</pre>
 *
 * <p>fills the table with three rows. One says that {@code Φ.t} is
 * complete, another that {@code Φ.t} has an attribute {@code next} whose
 * type is {@code Φ.t.next}, and the third that {@code Φ.t.next} is
 * complete as well, and has nothing.</p>
 *
 * <p>So a row is either about a type or about one of its attributes, and
 * which one it is can be seen from its cells: an attribute row carries
 * an {@code owner}. A type is identified by its locator, an attribute by
 * the locator of its owner and its own name, because those two are what
 * make it unique — its own locator would not do, since an attribute is
 * also a type in its own right, and the two rows would collide.</p>
 *
 * <p>"Complete" means that we have seen the whole formation, so there is
 * nothing in it besides the attributes listed. It is the flag that keeps
 * the checker honest later: a missing attribute is a mistake only when
 * the object that misses it is complete. An atom is not complete, since
 * its {@code λ} attribute stands for a body written in Java, which this
 * module cannot read. Neither is a formation that binds {@code φ}: what
 * it delegates to answers for every name it does not bind itself, and
 * that object is the business of the links table, not of this one.</p>
 *
 * <p>Two kinds of objects are deliberately absent from this table.
 * Applications and references (anything with a {@code @base}) provide
 * nothing on their own — what they have is what the object they copy
 * has, which is the business of the links table. And a void attribute
 * provides nothing either, until something is put into it.</p>
 *
 * <p>What an atom comes back with is written down, though its body is not.
 * {@code [] > div /Q.number} says that a {@code div} is a {@code Φ.number}
 * once it has run, and the parser carries that annotation into the XMIR, so
 * the row keeps it and whoever reads the table can ask a {@code number} what
 * the atom itself cannot answer. An annotation that names no object is
 * skipped: {@code [] > recovered /A} comes back with whatever the caller put
 * in, and that is a variable, which nothing here understands yet.</p>
 *
 * <p>Not every attribute is written inside the formation it belongs to.
 * {@code ρ}, the object something sits in, is written nowhere and every
 * object has one, which {@link Parents} reads off the locator. And
 * {@code minus} in the package {@code number} is {@code Φ.number.minus} and
 * belongs to {@code Φ.number} without ever appearing among its children,
 * which {@link Members} finds. Both go into this table after the attributes
 * a formation binds itself, since the order of those is what binds the
 * arguments of an application and neither of these two is one of them.</p>
 *
 * @since 0.67.0
 */
final class Provides implements Clue {

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        final Xmirs world = new Xmirs(xmirs);
        final Collection<XML> made = world.formations();
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            for (final XML formation : made) {
                final String owner = formation.xpath("@loc").get(0);
                final boolean whole = formation.nodes("o[@name='λ' or @name='φ']").isEmpty();
                rows.add(owner).set("complete", Boolean.toString(whole));
                for (final String back
                    : formation.xpath("o[@name='λ']/@atom[starts-with(., 'Φ.')]")) {
                    rows.add(owner).set("returns", back);
                }
                for (final XML attr : formation.nodes("o[@name and not(@name='λ')]")) {
                    final String name = attr.xpath("@name").get(0);
                    final Tojo row = rows.add(String.join(" ", owner, name))
                        .set("owner", owner)
                        .set("name", name)
                        .set("type", attr.xpath("@loc").get(0));
                    if (attr.xpath("@base").contains("∅")) {
                        row.set("void", "true");
                    }
                }
            }
            new Parents(made).fill(rows);
            new Members(made, world.roots()).fill(rows);
            Files.createDirectories(tables);
            Files.write(
                tables.resolve("provides.xml"),
                new Grouped(rows, "provides").asXml().toString().getBytes(StandardCharsets.UTF_8)
            );
        }
    }
}
