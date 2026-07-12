/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.util.function.Function;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Syntax parser that converts EO code to XMIR — the XML-based
 * Intermediate Representation. Builds the XMIR shell ({@code <object>}
 * root with version / revision / time / listing) via {@link DrProgram},
 * walks the source with the {@link Eo} spec-driven parser (see
 * {@code PARSER_SPEC.md}), then applies the canonical XSL chain to
 * normalise the output.
 * @since 0.1
 */
public final class EoSyntax implements Syntax {

    /**
     * Canonical XSL pipeline applied to the raw parser output.
     *
     * <p>This one is not aware of any objects, so bare references are
     * always homed into the root {@code Φ} package. Use
     * {@link #canonical(String)} to make the pipeline aware of the
     * objects that exist, so that same-package references can be
     * resolved automatically (see {@code add-default-package.xsl}).</p>
     */
    static final Function<XML, XML> CANONICAL = EoSyntax.canonical("");

    /**
     * Build the canonical XSL pipeline that is aware of the given
     * objects when homing bare references into their package.
     * @param objects Space separated list of fully qualified names of
     *  all the objects the compiler is aware of. A bare reference is
     *  resolved into the current package only if such an object exists
     *  there. May be empty, in which case bare references are always
     *  homed into the root {@code Φ}.
     * @return The transformation function
     */
    public static Function<XML, XML> canonical(final String objects) {
        return new Xsline(
            new TrFull(
                new TrJoined<>(
                    new TrClasspath<>(
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
                            String.format("objects %s", objects)
                        )
                    ),
                    new TrClasspath<>(
                        "/org/eolang/parser/parse/roll-bases.xsl",
                        "/org/eolang/parser/parse/cti-adds-errors.xsl",
                        "/org/eolang/parser/parse/decorate.xsl",
                        "/org/eolang/parser/parse/mandatory-as.xsl"
                    ).back(),
                    new TrDefault<>(new StHex())
                )
            )
        )::pass;
    }

    /**
     * Text to parse.
     */
    private final Input input;

    /**
     * Transform XMIR after parsing.
     */
    private final Function<XML, XML> transform;

    /**
     * Ctor.
     * @param ipt The EO program to parse
     */
    public EoSyntax(final String ipt) {
        this(new InputOf(ipt));
    }

    /**
     * Ctor.
     * @param ipt The EO program to parse
     * @param transform Transform XMIR after parsing
     */
    public EoSyntax(final String ipt, final Train<Shift> transform) {
        this(new InputOf(ipt), transform);
    }

    /**
     * Ctor.
     * @param ipt The EO program to parse
     */
    public EoSyntax(final Input ipt) {
        this(ipt, EoSyntax.CANONICAL);
    }

    /**
     * Ctor for testing.
     * @param ipt The EO program to parse
     * @param transform Transform XMIR after parsing train
     */
    EoSyntax(final Input ipt, final Train<Shift> transform) {
        this(ipt, new Xsline(transform)::pass);
    }

    /**
     * Ctor.
     * @param ipt The EO program to parse
     * @param transform Transform XMIR after parsing function
     */
    public EoSyntax(final Input ipt, final Function<XML, XML> transform) {
        this.input = ipt;
        this.transform = transform;
    }

    @Override
    public XML parsed() throws IOException {
        final String text = new UncheckedText(new TextOf(this.input)).asString();
        return this.transform.apply(
            new XMLDocument(
                new Xembler(
                    new Directives()
                        .append(new DrProgram())
                        .append(new Listing(text))
                        .xpath("/object")
                        .strict(1)
                        .append(new Eo(text).directives())
                        .attr("ms", 0L)
                        .up()
                ).domQuietly()
            )
        );
    }
}
