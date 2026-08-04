/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.eolang.parser.TrFull;

/**
 * The types of the objects of one XMIR document.
 *
 * <p>Give it the XMIR that the parser has already canonicalised (see
 * {@code org.eolang.parser.Canonical}) and it answers with the tables
 * the checker keeps. It's as simple as this:</p>
 *
 * <pre> XML rows = new Inference(xmir).provides();</pre>
 *
 * <p>Two things happen to the XMIR before any rule looks at it. First,
 * every composite base is split into one object per dispatch step: the
 * parser rolls {@code x.next.foo} into a single base, while a rule can
 * only notice an attribute being taken when the taking is an object of
 * its own. Second, the locators are set again, because the objects that
 * the splitting has just created have none.</p>
 *
 * <p>The locator is also what this module uses as the identity of a
 * type. The design it follows numbers the objects instead ({@code t0},
 * {@code t1}, and so on), but XMIR already gives every object a unique
 * name — {@code Φ.app.inc.φ.ρ} — and a table saying that
 * {@code Φ.app.t.next} has nothing needs no dictionary to be read by a
 * human. The rule that matters holds either way: a type belongs to one
 * object, and is never renamed or merged with another.</p>
 *
 * @since 0.67.0
 */
public final class Inference {

    /**
     * The prepared XMIR, made once and remembered.
     */
    private final Unchecked<XML> xmir;

    /**
     * Ctor.
     * @param canonical The XMIR, as the parser leaves it after the
     *  canonical pipeline
     */
    public Inference(final XML canonical) {
        this.xmir = new Unchecked<>(
            new Sticky<>(
                () -> new Xsline(
                    new TrFull(
                        new TrDefault<>(
                            new StClasspath("/org/eolang/inference/unroll-bases.xsl"),
                            new StClasspath("/org/eolang/parser/parse/set-locators.xsl")
                        )
                    )
                ).pass(canonical)
            )
        );
    }

    /**
     * The XMIR as this module sees it.
     * @return XMIR with one object per dispatch step, and a locator on
     *  every object
     */
    public XML prepared() {
        return this.xmir.value();
    }

    /**
     * What every object certainly has.
     * @return The provides table
     */
    public XML provides() {
        return new Provides(this.xmir.value()).asXml();
    }
}
