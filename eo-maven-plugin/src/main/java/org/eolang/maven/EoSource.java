/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.eolang.parser.EoSyntax;
import org.eolang.parser.OnDefault;
import org.eolang.parser.OnDetailed;
import org.w3c.dom.Node;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * EO source code for a single EO object.
 * @since 0.60
 */
final class EoSource {

    /**
     * Object identifier.
     */
    private final String identifier;

    /**
     * Object source code.
     */
    private final Input input;

    /**
     * Ctor.
     * @param identifier Object identifier
     * @param source Path to the source file
     */
    EoSource(final String identifier, final Path source) {
        this(identifier, new InputOf(source));
    }

    /**
     * Ctor.
     * @param identifier Object identifier
     * @param input Object source code
     */
    EoSource(final String identifier, final Input input) {
        this.identifier = identifier;
        this.input = input;
    }

    /**
     * Parse the source code into XMIR.
     * @return Parsed XMIR
     * @throws IOException If fails
     */
    Xmir parsed() throws IOException {
        final XML xmir = new EoSyntax(this.input).parsed();
        final List<String> errors = new ArrayList<>(0);
        final Node document = xmir.inner();
        final String name = new OnDetailed(
            new OnDefault(xmir),
            e -> EoSource.applyError("mandatory-object-name", e.getMessage(), document)
        ).get();
        if (!name.equals(this.identifier)) {
            final String msg = Logger.format(
                "For some reason, the identifier of the tojo, which essentially is a name of the source file ('%s'), does not match the name of the object discovered in the XMIR after parsing ('%s')",
                this.identifier,
                name
            );
            EoSource.applyError("validate-object-name", msg, document);
            errors.add(msg);
        }
        return new Xmir(new XMLDocument(document), errors);
    }

    /**
     * Apply error to the document.
     * @param check Check name
     * @param message Error message
     * @param document Document
     */
    private static void applyError(
        final String check, final String message, final Node document
    ) {
        new Xembler(
            new Directives().xpath("/object").addIf("errors").add("error").attr("check", check)
                .attr("severity", "critical").set(message)
        ).applyQuietly(document);
    }

    /**
     * Parsing result as XMIR with possible errors.
     * @since 0.60
     */
    static class Xmir {

        /**
         * Resulting XML.
         */
        private final XML res;

        /**
         * List of errors.
         */
        private final List<String> errors;

        /**
         * Ctor.
         * @param res Resulting XML
         * @param errors List of errors
         */
        Xmir(final XML res, final List<String> errors) {
            this.res = res;
            this.errors = errors;
        }

        /**
         * Resulting XML.
         * @return XML xmir representation of the EO object
         */
        XML xml() {
            return this.res;
        }

        /**
         * Is the XMIR broken.
         * @return Is broken or not
         */
        boolean broken() {
            return !this.errors.isEmpty();
        }
    }
}
