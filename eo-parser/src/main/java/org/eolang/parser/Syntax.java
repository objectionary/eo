/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import java.io.IOException;

/**
 * Syntax parser, to XMIR, using ANTLR4.
 * @since 0.34.0
 */
interface Syntax {
    /**
     * Parse it to XML.
     * @return Parsed XML
     * @throws IOException If fails
     */
    XML parsed() throws IOException;
}
