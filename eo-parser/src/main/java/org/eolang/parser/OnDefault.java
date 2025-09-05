/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;

/**
 * Function that builds object name from:
 * 1. /object/metas/meta[head='package']/tail/text()
 * 2. /object/o/@name
 * <p>If package is present - it'll be joined with object name by dot.
 * Otherwise, only object name without package is returned.</p>
 * @since 0.52
 */
public final class OnDefault implements ObjectName {
    /**
     * Navigator.
     */
    private final Xnav xnav;

    /**
     * Ctor.
     * @param xml XML
     */
    public OnDefault(final XML xml) {
        this(new Xnav(xml.inner()));
    }

    /**
     * Ctor.
     * @param nav Navigator
     */
    public OnDefault(final Xnav nav) {
        this.xnav = nav;
    }

    @Override
    public String get() {
        final String obj = this.xnav
            .element("object")
            .element("o")
            .attribute("name")
            .text()
            .orElseThrow(
                () -> new IllegalStateException("XMIR should have '/object/o/@name' attribute")
            );
        return this.xnav.element("object")
            .elements(Filter.withName("metas"))
            .findFirst()
            .map(
                metas -> metas.elements(
                    Filter.all(
                        Filter.withName("meta"),
                        meta -> new Xnav(meta)
                            .element("head")
                            .text()
                            .map("package"::equals)
                            .orElse(false)
                    )
                )
                .findFirst()
                .map(
                    meta -> meta.element("tail").text().map(
                        pckg -> String.join(".", pckg, obj)
                    ).orElse(obj)
                )
                .orElse(obj)
            )
            .orElse(obj);
    }
}
