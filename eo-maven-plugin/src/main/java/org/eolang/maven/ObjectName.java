/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.function.Supplier;

/**
 * Function that builds object name from:
 * 1. /object/metas/meta[head='package']/tail/text()
 * 2. /object/o/@name
 * @since 0.52
 */
final class ObjectName implements Supplier<String> {
    /**
     * Navigator.
     */
    private final Xnav xnav;

    /**
     * Ctor.
     * @param xml XML
     */
    ObjectName(final XML xml) {
        this.xnav = new Xnav(xml.inner());
    }

    @Override
    public String get() {
        final String pckg = this.xnav.element("object")
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
                .map(meta -> meta.element("tail").text().orElse(""))
                .orElse("")
            )
            .orElse("");
        final String obj = this.xnav
            .element("object")
            .element("o")
            .attribute("name")
            .text()
            .orElse("");
        final String name;
        if (pckg.isEmpty()) {
            name = obj;
        } else {
            name = String.join(".", pckg, obj);
        }
        return name;
    }
}
