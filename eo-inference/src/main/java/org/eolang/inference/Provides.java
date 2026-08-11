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
 * <p>Three kinds of objects are deliberately absent from this table.
 * Applications and references (anything with a {@code @base}) provide
 * nothing on their own — what they have is what the object they copy
 * has, which is the business of the links table. A void attribute
 * provides nothing either, until something is put into it. And the
 * results of atoms are not here at all yet: what an atom returns is
 * written in Java, so its rows will have to be given to this table from
 * outside one day, and until they are, the checker simply knows less.</p>
 *
 * @since 0.67.0
 * @todo #6565:35min Account for attributes reached through the package.
 *  An attribute of {@code Φ.number} like {@code minus} lives in the same
 *  package as a top-level object {@code Φ.number.minus}, so it never
 *  shows up among the children of the formation and this table calls the
 *  object complete while listing a strict subset of what it has. Either
 *  write those package members down as attributes of their owner, or
 *  leave the owner incomplete until they are.
 * @todo #6565:35min Write down the {@code ρ} every object has. An outer
 *  name lowers to a dispatch of {@code ρ}, so the needs table asks for
 *  it, while no row here ever lists it and the owner is called complete
 *  all the same. Give every type a {@code ρ} attribute, or say in this
 *  table that {@code ρ} is not a name the checker judges.
 */
final class Provides implements Clue {

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            int seen = 0;
            for (final XML formation : new Xmirs(xmirs).formations()) {
                final String owner = formation.xpath("@loc").get(0);
                final boolean whole = formation.nodes("o[@name='λ' or @name='φ']").isEmpty();
                rows.add(owner)
                    .set("index", Integer.toString(seen))
                    .set("complete", Boolean.toString(whole));
                seen = seen + 1;
                for (final XML attr : formation.nodes("o[@name and not(@name='λ')]")) {
                    final String name = attr.xpath("@name").get(0);
                    final Tojo row = rows.add(String.join(" ", owner, name))
                        .set("owner", owner)
                        .set("index", Integer.toString(seen))
                        .set("name", name)
                        .set("type", attr.xpath("@loc").get(0));
                    if (attr.xpath("@base").contains("∅")) {
                        row.set("void", "true");
                    }
                    seen = seen + 1;
                }
            }
            Files.createDirectories(tables);
            Files.write(
                tables.resolve("provides.xml"),
                new Grouped(rows, "provides").asXml().toString().getBytes(StandardCharsets.UTF_8)
            );
        }
    }
}
