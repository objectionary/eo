/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import java.util.function.Supplier;
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
     * Program name.
     */
    private final Supplier<String> name;

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
        this(
            () -> file.getFileName().toString().replace(".phi", ""),
            file,
            metas
        );
    }

    /**
     * Ctor.
     * @param source PHI source code
     * @param metas Extra metas to add after parsing
     */
    public Phi(final Text source, final Iterable<Directive> metas) {
        this(() -> "unknown", source, metas);
    }

    /**
     * Ctor.
     * @param name Program name
     * @param file Source file
     * @param metas Extra metas to add after parsing
     */
    public Phi(final Supplier<String> name, final Path file, final Iterable<Directive> metas) {
        this(name, new TextOf(file), metas);
    }

    /**
     * Ctor.
     * @param name Program name
     * @param source PHI source code
     * @param metas Extra metas to add after parsing
     */
    public Phi(final Supplier<String> name, final Text source, final Iterable<Directive> metas) {
        this.name = name;
        this.source = source;
        this.metas = metas;
    }

    /**
     * Parse PHI expression to XMIR.
     * @return Parsed PHI to XMIR.
     * @throws IOException If fails to parse
     */
    public XML unphi() throws IOException {
        return new PhiSyntax(this.name.get(), this.source, this.metas).parsed();
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
                                .xpath("/program")
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
