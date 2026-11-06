/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.IOException;
import java.nio.file.Path;
import org.xembly.Directives;

/**
 * One kind of fact a program states about itself.
 *
 * <p>A fact is what can be read off the text before anything is worked out:
 * what a call passes, what an object is asked for, what it has. Every kind
 * goes into one document, a section each, because the facts are read
 * together later and a reader that opens one file sees all of them. A
 * section does not write that document, it only says what goes into its own
 * part of it, and {@link Facts} writes the whole once, so no section can
 * write over another.</p>
 *
 * <p>What comes back starts with the element that holds the section and
 * leaves the cursor where it found it, since the next section starts
 * beside it.</p>
 *
 * @since 0.76.0
 */
@FunctionalInterface
interface Section {

    /**
     * This section as read off the program.
     *
     * @param xmirs The directory with the prepared XMIR of the whole program
     * @return The directives
     * @throws IOException If a file cannot be read
     */
    Directives directives(Path xmirs) throws IOException;
}
