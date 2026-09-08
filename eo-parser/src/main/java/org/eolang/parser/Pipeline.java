/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.util.function.UnaryOperator;
import org.cactoos.Scalar;

/**
 * The scalar that builds the canonical pipeline out of
 * {@link Canonical#XSLS}.
 *
 * @since 0.60
 */
final class Pipeline implements Scalar<UnaryOperator<XML>> {

    /**
     * The position in {@link Canonical#XSLS} of
     * {@code add-default-package.xsl}, the only stylesheet of the
     * pipeline that takes a parameter and therefore can't travel in a
     * plain {@link TrClasspath} run, but has to be wrapped into an
     * {@link StClasspath} of its own.
     */
    private static final int PACKAGED = Canonical.XSLS.indexOf(
        "/org/eolang/parser/parse/add-default-package.xsl"
    );

    /**
     * Space separated qualified names of the local package objects.
     */
    private final String objects;

    /**
     * Ctor.
     *
     * @param objs Space separated qualified names of local package objects
     */
    Pipeline(final String objs) {
        this.objects = objs;
    }

    @Override
    public UnaryOperator<XML> value() {
        return new Xsline(
            new TrFull(
                new TrJoined<>(
                    Pipeline.classpath(0, Pipeline.PACKAGED),
                    new TrDefault<Shift>(
                        new StClasspath(
                            Canonical.XSLS.get(Pipeline.PACKAGED),
                            String.format("objects %s", this.objects)
                        )
                    ),
                    Pipeline.classpath(
                        Pipeline.PACKAGED + 1, Canonical.XSLS.size() - 1
                    ),
                    new TrDefault<>(new StHex()),
                    Pipeline.classpath(
                        Canonical.XSLS.size() - 1, Canonical.XSLS.size()
                    )
                )
            )
        )::pass;
    }

    private static Train<Shift> classpath(final int from, final int till) {
        return new TrClasspath<Shift>(
            Canonical.XSLS.subList(from, till).toArray(new String[0])
        ).back();
    }
}
