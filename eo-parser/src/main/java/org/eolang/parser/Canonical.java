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
import com.yegor256.xsline.Xsline;
import java.util.function.UnaryOperator;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;

/**
 * The canonical XSL pipeline applied to the raw parser output.
 *
 * <p>The pipeline may be aware of the local package objects. When it
 * is, a bare reference in a program with a {@code +package} meta is
 * homed into the current package (as if there was a {@code +alias}
 * meta), but only when the name is one of those local package objects.
 * Otherwise the reference stays at the root {@code Φ} (it is treated
 * as a global). The awareness is passed to
 * {@code add-default-package.xsl} through the {@code objects}
 * parameter. When nothing is known, bare references are always homed
 * into the root {@code Φ} (see {@code add-default-package.xsl}).</p>
 *
 * @since 0.60
 */
public final class Canonical implements UnaryOperator<XML> {

    /**
     * The pipeline, built lazily and only once.
     */
    private final Unchecked<UnaryOperator<XML>> pipeline;

    /**
     * Ctor, not aware of any objects.
     */
    public Canonical() {
        this("");
    }

    /**
     * Ctor.
     * @param objects Space separated qualified names ("package.name")
     *  of the local package objects the compiler is aware of; a bare
     *  reference is homed into the current package only if
     *  "package.name" is one of them, otherwise it goes to the root
     *  {@code Φ}
     */
    public Canonical(final String objects) {
        this.pipeline = new Unchecked<>(
            new Synced<>(new Sticky<>(new Canonical.Pipeline(objects)))
        );
    }

    @Override
    public XML apply(final XML xml) {
        return this.pipeline.value().apply(xml);
    }

    /**
     * The scalar that builds the canonical pipeline.
     * @since 0.60
     */
    private static final class Pipeline implements Scalar<UnaryOperator<XML>> {

        /**
         * Space separated qualified names of the local package objects.
         */
        private final String objects;

        /**
         * Ctor.
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
                        new TrClasspath<>(
                            "/org/eolang/parser/parse/wrap-applications.xsl",
                            "/org/eolang/parser/parse/resolve-self.xsl",
                            "/org/eolang/parser/parse/resolve-local-names.xsl",
                            "/org/eolang/parser/parse/validate-before-stars.xsl",
                            "/org/eolang/parser/parse/resolve-before-stars.xsl",
                            "/org/eolang/parser/parse/fragile-dispatch.xsl",
                            "/org/eolang/parser/parse/wrap-method-calls.xsl",
                            "/org/eolang/parser/parse/const-to-dataized.xsl",
                            "/org/eolang/parser/parse/stars-to-tuples.xsl",
                            "/org/eolang/parser/parse/vars-float-up.xsl",
                            "/org/eolang/parser/parse/move-voids-up.xsl",
                            "/org/eolang/parser/parse/validate-objects-count.xsl",
                            "/org/eolang/parser/parse/build-fqns.xsl",
                            "/org/eolang/parser/parse/expand-aliases.xsl",
                            "/org/eolang/parser/parse/resolve-aliases.xsl"
                        ).back(),
                        new TrDefault<Shift>(
                            new StClasspath(
                                "/org/eolang/parser/parse/add-default-package.xsl",
                                String.format("objects %s", this.objects)
                            )
                        ),
                        new TrClasspath<>(
                            "/org/eolang/parser/parse/roll-bases.xsl",
                            "/org/eolang/parser/parse/cti-adds-errors.xsl",
                            "/org/eolang/parser/parse/mandatory-as.xsl"
                        ).back(),
                        new TrDefault<>(new StHex()),
                        new TrClasspath<>(
                            "/org/eolang/parser/parse/set-locators.xsl"
                        ).back()
                    )
                )
            )::pass;
        }
    }
}
