/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import org.cactoos.Text;
import org.cactoos.iterable.IterableEnvelope;
import org.cactoos.iterable.Joined;
import org.cactoos.iterable.Mapped;
import org.cactoos.text.TextOf;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Phi expression.
 * @since 0.51.0
 */
public class Phi {
    /**
     * Phi source code.
     */
    private final Text source;

    /**
     * Set of metas that are have to be added to result XMIR.
     */
    private final Iterable<Directive> metas;

    /**
     * Ctor.
     * @param file Source file
     */
    public Phi(final Path file) {
        this(file, new Directives());
    }

    /**
     * Ctor.
     * @param file Source file
     * @param metas Set of metas to add to result XMIR
     */
    public Phi(final Path file, final Set<String> metas) {
        this(file, new Phi.Metas(metas));
    }

    /**
     * Ctor.
     * @param source PHI source code.
     */
    public Phi(final String source) {
        this(new TextOf(source));
    }

    /**
     * Ctor.
     * @param source PHI source code
     */
    public Phi(final Text source) {
        this(source, new Directives());
    }

    /**
     * Ctor.
     * @param source PHI source code
     * @param metas Set of metas to add to result XMIR
     */
    public Phi(final Text source, final Set<String> metas) {
        this(source, new Phi.Metas(metas));
    }

    /**
     * Ctor.
     * @param file Source file
     * @param metas Extra metas to add after parsing
     */
    public Phi(final Path file, final Iterable<Directive> metas) {
        this(new TextOf(file), metas);
    }

    /**
     * Ctor.
     * @param source PHI source code
     * @param metas Extra metas to add after parsing
     */
    public Phi(final Text source, final Iterable<Directive> metas) {
        this.source = source;
        this.metas = metas;
    }

    /**
     * Parse PHI expression to XMIR.
     * @return Parsed PHI to XMIR.
     * @throws IOException If fails to parse
     */
    public XML unphi() throws IOException {
        return new PhiSyntax(this.source, this.metas).parsed();
    }

    /**
     * Accumulates all metas that should be attached to unphied XMIR.
     * +package meta is prohibited since it's converted to special object
     * with "λ -> Package" binding.
     * @since 0.36.0
     */
    public static class Metas extends IterableEnvelope<Directive> {
        /**
         * Package meta.
         */
        private static final String PACKAGE = "package";

        /**
         * Ctor.
         * @param metas Metas to append
         */
        public Metas(final Iterable<String> metas) {
            super(
                new Joined<>(
                    new Mapped<>(
                        meta -> {
                            final String[] pair = meta.split(" ", 2);
                            final String head = pair[0].substring(1);
                            if (Phi.Metas.PACKAGE.equals(head)) {
                                throw new IllegalStateException(
                                    "+package meta is prohibited for attaching to unphied XMIR"
                                );
                            }
                            final Directives dirs = new Directives()
                                .xpath("/object")
                                .addIf("metas")
                                .add("meta")
                                .add("head").set(head).up()
                                .add("tail");
                            if (pair.length > 1) {
                                dirs.set(pair[1].trim()).up();
                                for (final String part : pair[1].trim().split(" ")) {
                                    dirs.add("part").set(part).up();
                                }
                            } else {
                                dirs.up();
                            }
                            return dirs.up();
                        },
                        metas
                    )
                )
            );
        }
    }
}
