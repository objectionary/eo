/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import java.util.List;
import java.util.function.UnaryOperator;
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
     * The classpath resources of every XSL this pipeline applies, in the
     * order they run, so a caller that caches this pipeline's output (see
     * {@code Parsing} in {@code eo-maven-plugin}, #6236) can fold a
     * fingerprint of them into its cache key. This very list also builds
     * the pipeline in {@link Pipeline#value()}, so the two can
     * never drift apart, the same way {@code Transpilation.XSLS} feeds
     * both the train and the cache key in {@code eo-maven-plugin}. An
     * immutable {@link List}, not an array, since this is exported as
     * public API of eo-parser and an array field would let any caller
     * silently corrupt it for everyone.
     */
    public static final List<String> XSLS = List.of(
        "/org/eolang/parser/parse/wrap-applications.xsl",
        "/org/eolang/parser/parse/resolve-local-names.xsl",
        "/org/eolang/parser/parse/validate-bindings.xsl",
        "/org/eolang/parser/parse/resolve-before-stars.xsl",
        "/org/eolang/parser/parse/fragile-dispatch.xsl",
        "/org/eolang/parser/parse/wrap-method-calls.xsl",
        "/org/eolang/parser/parse/const-to-dataized.xsl",
        "/org/eolang/parser/parse/stars-to-tuples.xsl",
        "/org/eolang/parser/parse/vars-float-up.xsl",
        "/org/eolang/parser/parse/validate-objects-count.xsl",
        "/org/eolang/parser/parse/validate-object-presence.xsl",
        "/org/eolang/parser/parse/validate-attribute-names.xsl",
        "/org/eolang/parser/parse/validate-voids.xsl",
        "/org/eolang/parser/parse/build-fqns.xsl",
        "/org/eolang/parser/parse/expand-aliases.xsl",
        "/org/eolang/parser/parse/validate-aliases.xsl",
        "/org/eolang/parser/parse/resolve-aliases.xsl",
        "/org/eolang/parser/parse/add-default-package.xsl",
        "/org/eolang/parser/parse/roll-bases.xsl",
        "/org/eolang/parser/parse/mandatory-as.xsl",
        "/org/eolang/parser/parse/set-locators.xsl"
    );

    /**
     * Classpath resources {@code xsl:import}-ed by one or more of
     * {@link #XSLS} (const-to-dataized, vars-float-up,
     * build-fqns, add-default-package, roll-bases and set-locators,
     * confirmed by grepping their {@code xsl:import}s), so
     * their content must also be folded into a fingerprint that means to
     * catch every change to the pipeline's actual output — editing one of
     * these shared libraries changes what {@link #XSLS} produces just as
     * much as editing a top-level stylesheet does, but leaves {@link #XSLS}
     * itself byte-for-byte unchanged. Mirrors {@code Transpilation.IMPORTS}
     * in {@code eo-maven-plugin} (#6032), which closed the same gap for the
     * transpile cache key.
     */
    public static final List<String> IMPORTS = List.of(
        "/org/eolang/parser/_funcs.xsl",
        "/org/eolang/parser/_specials.xsl"
    );

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
            new Synced<>(new Sticky<>(new Pipeline(objects)))
        );
    }

    @Override
    public XML apply(final XML xml) {
        return this.pipeline.value().apply(xml);
    }
}
