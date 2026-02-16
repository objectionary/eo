/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.Optional;

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
        return this.xnav.element("object")
            .elements(Filter.withName("metas"))
            .findFirst()
            .map(this::joined)
            .orElseGet(this::fallback);
    }

    /**
     * Find object name from metas.
     * @param metas Metas navigator
     * @return Object name with package if found
     */
    private String joined(final Xnav metas) {
        return metas.elements(
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
                pckg -> String.join(".", pckg, this.fallback())
            ).orElseGet(this::fallback)
        )
        .orElseGet(this::fallback);
    }

    /**
     * Get fallback object name.
     * @return Object name without package
     */
    private String fallback() {
        return this.name()
            .orElseThrow(
                () -> new IllegalStateException(
                    "XMIR should have either '/object/o/@name' or '/object/class/@name' attribute"
                )
            );
    }

    /**
     * Get object name from XMIR.
     * @return Object name
     */
    private Optional<String> name() {
        return Optional.ofNullable(
            this.xnav
                .element("object")
                .element("o")
                .attribute("name")
                .text().orElseGet(
                    () -> this.xnav.path("/object/class/@name")
                        .findFirst()
                        .flatMap(Xnav::text)
                        .orElse(null)
                )
        );
    }
}
