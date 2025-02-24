/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.function.Supplier;

/**
 * Function that extract "/program/@name" from XML.
 * @since 0.52
 */
final class ProgramName implements Supplier<String> {
    /**
     * Navigator.
     */
    private final Xnav xnav;

    /**
     * Ctor.
     * @param xml XML
     */
    ProgramName(final XML xml) {
        this.xnav = new Xnav(xml.inner());
    }

    @Override
    public String get() {
        return this.xnav.element("program").attribute("name").text().orElse("");
    }
}
