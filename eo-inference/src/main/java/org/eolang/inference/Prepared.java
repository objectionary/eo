/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.util.function.UnaryOperator;
import org.eolang.parser.TrFull;

/**
 * The XMIR of one file, made ready for the facts.
 *
 * <p>Two things happen to a file the parser wrote before {@link Facts}
 * reads it. First, every composite base is split into one object per name,
 * the first name too: the parser rolls {@code x.next.foo} into a single
 * base, while a fact can only say that an attribute is read where the
 * reading is an object of its own, so the chain comes apart into
 * {@code .foo} taken off {@code .next}, taken off {@code .x}, taken off a
 * bare {@code ξ}. Second, the locators are set again, because the objects
 * the split has just made have none, and every fact points at an object
 * by its locator.</p>
 *
 * <p>The Maven plugin prepares the files of a build here, and the test
 * packs of this module prepare theirs the same way, so the facts a pack
 * expects are the facts a build writes.</p>
 *
 * @since 0.76.0
 */
public final class Prepared implements UnaryOperator<XML> {

    /**
     * The train the file goes through.
     */
    private final Xsline train;

    /**
     * Ctor.
     */
    public Prepared() {
        this(
            new Xsline(
                new TrFull(
                    new TrDefault<>(
                        new StClasspath("/org/eolang/inference/unroll-bases.xsl"),
                        new StClasspath("/org/eolang/parser/parse/set-locators.xsl")
                    )
                )
            )
        );
    }

    /**
     * Ctor.
     *
     * @param line The train the file goes through
     */
    Prepared(final Xsline line) {
        this.train = line;
    }

    @Override
    public XML apply(final XML xmir) {
        return this.train.pass(xmir);
    }
}
