/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjCached;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.util.List;

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
 * <p>Every row also remembers when it was written, in {@code index}. A
 * table is a set of rows and promises nothing about their order, while
 * the order of attributes is not decoration here: an application binds
 * its arguments to the void attributes of a formation in the order they
 * were declared, so the rule that checks applications will ask for the
 * first void and must get the same answer every time. Counting the rows
 * as they are written keeps that, and keeps the report following the
 * code rather than the whims of a hash table.</p>
 *
 * <p>"Complete" means that we have seen the whole formation, so there is
 * nothing in it besides the attributes listed. It is the flag that keeps
 * the checker honest later: a missing attribute is a mistake only when
 * the object that misses it is complete. An atom is not complete, since
 * its {@code λ} attribute stands for a body written in Java, which this
 * module cannot read.</p>
 *
 * <p>Three kinds of objects are deliberately absent from this table.
 * Applications and references (anything with a {@code @base}) provide
 * nothing on their own — what they have is what the object they copy
 * has, which is the business of the links table. A void attribute
 * provides nothing either, until something is put into it. And objects
 * from other files, together with the results of atoms, are not here at
 * all yet; the design expects them to arrive one day as ready-made rows,
 * and until they do, the checker simply knows less.</p>
 *
 * @since 0.67.0
 */
final class Provides implements Table {

    /**
     * The prepared XMIR.
     */
    private final XML xmir;

    /**
     * Ctor.
     * @param prepared The XMIR, as {@link Inference#prepared()} leaves it
     */
    Provides(final XML prepared) {
        this.xmir = prepared;
    }

    @Override
    public Tojos rows() {
        final Tojos rows = new TjCached(new TjDefault(new MnMemory()));
        int seen = 0;
        for (final XML formation : this.formations()) {
            final String owner = formation.xpath("@loc").get(0);
            rows.add(owner)
                .set("index", Integer.toString(seen))
                .set("complete", Boolean.toString(formation.nodes("o[@name='λ']").isEmpty()));
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
        return rows;
    }

    /**
     * Every formation of the program.
     *
     * <p>A formation is an object with no {@code @base}: it is not a copy
     * of anything, it is written down as it is. Two other kinds of
     * objects have no base either and are not formations: data, which
     * carries its bytes as text, and the {@code λ} marker of an atom,
     * which names a body implemented in Java.</p>
     *
     * @return The formations, in the order they appear in the code
     */
    private List<XML> formations() {
        return this.xmir.nodes(
            "//o[not(@base) and not(@name='λ') and not(text()[normalize-space()])]"
        );
    }
}
