/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import org.xembly.Directives;
import org.xembly.Xembler;

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
 * <p>gives two rows: {@code Φ.t} has {@code next} (whose type is
 * {@code Φ.t.next}), and {@code Φ.t.next} has nothing. Both are
 * complete.</p>
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
     * Every formation of the program.
     *
     * <p>A formation is an object with no {@code @base}: it is not a copy
     * of anything, it is written down as it is. Two other kinds of
     * objects have no base either and are not formations: data, which
     * carries its bytes as text, and the {@code λ} marker of an atom,
     * which names a body implemented in Java.</p>
     */
    private static final String FORMATIONS =
        "//o[not(@base) and not(@name='λ') and not(text()[normalize-space()])]";

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
    public XML asXml() {
        final Directives dirs = new Directives().add("provides");
        for (final XML formation : this.xmir.nodes(Provides.FORMATIONS)) {
            dirs.add("type")
                .attr("id", formation.xpath("@loc").get(0))
                .attr("complete", Provides.mark(formation.nodes("o[@name='λ']").isEmpty()));
            for (final XML attr : formation.nodes("o[@name and not(@name='λ')]")) {
                dirs.add("attr")
                    .attr("name", attr.xpath("@name").get(0))
                    .attr("type", attr.xpath("@loc").get(0));
                if (attr.xpath("@base").contains("∅")) {
                    dirs.attr("void", "yes");
                }
                dirs.up();
            }
            dirs.up();
        }
        return new XMLDocument(new Xembler(dirs).domQuietly());
    }

    /**
     * The way this table spells a flag.
     * @param flag The flag
     * @return Either "yes" or "no"
     */
    private static String mark(final boolean flag) {
        final String mark;
        if (flag) {
            mark = "yes";
        } else {
            mark = "no";
        }
        return mark;
    }
}
