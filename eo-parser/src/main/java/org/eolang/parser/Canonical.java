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
import java.util.List;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
     * Classpath XSLs applied before the local-package-aware
     * default-package stage, in pipeline order.
     */
    static final List<String> HEAD = List.of(
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
    );

    /**
     * The classpath XSL that homes a bare reference into the current
     * package, applied with a runtime {@code objects} parameter.
     */
    static final String DEFAULT_PACKAGE = "/org/eolang/parser/parse/add-default-package.xsl";

    /**
     * Classpath XSLs applied after the default-package stage and before
     * hex-string decoding, in pipeline order.
     */
    static final List<String> MID = List.of(
        "/org/eolang/parser/parse/roll-bases.xsl",
        "/org/eolang/parser/parse/cti-adds-errors.xsl",
        "/org/eolang/parser/parse/mandatory-as.xsl"
    );

    /**
     * The classpath XSL applied last, after hex-string decoding.
     */
    static final String TAIL = "/org/eolang/parser/parse/set-locators.xsl";

    /**
     * Shared library XSLs the stages above {@code xsl:import}.
     */
    static final List<String> IMPORTS = List.of(
        "/org/eolang/parser/_funcs.xsl",
        "/org/eolang/parser/_specials.xsl"
    );

    /**
     * All classpath XSLs the canonical pipeline applies or imports, in
     * pipeline order. Folded into the parse cache key's fingerprint by
     * {@code org.eolang.maven.Parsing} so that editing any of them
     * invalidates previously cached XMIR for the same source, the same
     * way {@code Transpilation.XSLS}/{@code Transpilation.IMPORTS} are
     * folded into the transpile cache key. Derived from the same group
     * constants {@link Pipeline#value()} builds the real pipeline from,
     * so the two cannot drift apart.
     */
    public static final List<String> XSLS = Stream.of(
        Canonical.HEAD.stream(),
        Stream.of(Canonical.DEFAULT_PACKAGE),
        Canonical.MID.stream(),
        Stream.of(Canonical.TAIL),
        Canonical.IMPORTS.stream()
    ).flatMap(s -> s).collect(Collectors.toUnmodifiableList());

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
                            Canonical.HEAD.toArray(new String[0])
                        ).back(),
                        new TrDefault<Shift>(
                            new StClasspath(
                                Canonical.DEFAULT_PACKAGE,
                                String.format("objects %s", this.objects)
                            )
                        ),
                        new TrClasspath<>(
                            Canonical.MID.toArray(new String[0])
                        ).back(),
                        new TrDefault<>(new StHex()),
                        new TrClasspath<>(Canonical.TAIL).back()
                    )
                )
            )::pass;
        }
    }
}
