/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import java.nio.file.Path;
import org.cactoos.func.FuncEnvelope;

/**
 * The place for EO program constructed from its name taken from XMIR.
 * @since 0.51
 */
final class ProgramPlace extends FuncEnvelope<XML, Path> {
    /**
     * Ctor.
     * @param dir Home directory
     */
    ProgramPlace(final Path dir) {
        super(
            xml -> new Place(new ProgramName(xml).get()).make(dir, "")
        );
    }
}
