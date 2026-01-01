/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import java.io.IOException;

/**
 * Core interface for syntax parsers that transform source code into the XMIR
 * (XML-based Intermediate Representation) format using ANTLR4.
 *
 * <p>This interface abstracts the parsing process for different syntaxes in the
 * EO ecosystem, such as EO language. Implementations typically perform lexical analysis,
 * syntax parsing, and XMIR generation through a series of XSL transformations to produce canonical
 * XML output.</p>
 *
 * <p>The interface is designed to be lightweight with a single method {@code parsed()}
 * that handles the entire conversion process.</p>
 *
 * @see org.eolang.parser.EoSyntax Implementation for EO language
 * @since 0.34.0
 */
interface Syntax {
    /**
     * Parses the source input and transforms it into XMIR format.
     *
     * <p>This method handles the complete parsing workflow: lexical analysis,
     * syntax analysis, and transformation to XML. The resulting XML follows the
     * canonical XMIR structure defined by the EO project.</p>
     *
     * @return Parsed XMIR document representing the input source
     * @throws IOException If parsing fails due to syntax errors, I/O issues,
     *  or transformation problems
     */
    XML parsed() throws IOException;
}
