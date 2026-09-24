/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * Objectionary URL template.
 *
 * <p>Assumes two placeholders in terms of
 * {@link String#format(String, Object...)}: 1st for version hash,
 * 2nd for program or directory name, for
 * <a href="https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo">programExample</a>
 * or
 * <a href="https://github.com/objectionary/home/tree/%s/objects/%s">directoryExample</a>.</p>
 *
 * @since 0.1.0
 */
final class UrlOy {

    /**
     * URL template.
     *
     * <p>Expects two placeholders in terms of
     * {@link String#format(String, Object...)}: 1st for hash,
     * 2nd for program or directory name, for
     * <a href="https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo">programExample</a>
     * or
     * <a href="https://github.com/objectionary/home/tree/%s/objects/%s">directoryExample</a>.</p>
     */
    private final String template;

    /**
     * Objects version hash.
     */
    private final CommitHash hash;

    /**
     * Ctor for testing.
     *
     * @param template URL template
     * @param hash Commit hash
     */
    UrlOy(final String template, final String hash) {
        this(template, () -> hash);
    }

    /**
     * Ctor.
     *
     * @param template URL template
     * @param hash Objects version hash
     */
    UrlOy(final String template, final CommitHash hash) {
        this.template = template;
        this.hash = hash;
    }

    @Override
    public String toString() {
        return this.template;
    }

    /**
     * URL for the program or directory.
     *
     * @param name Fully qualified EO program as specified by {@link Place} or directory name
     * @return URL
     * @throws MalformedURLException in case of incorrect URL
     */
    URL value(final String name) throws MalformedURLException {
        final String prefix = "org.eolang.";
        final String stripped;
        if (name.startsWith(prefix)) {
            stripped = name.substring(prefix.length());
        } else {
            stripped = name;
        }
        return new URL(
            String.format(
                this.template,
                this.hash.value(),
                stripped.replace(".", "/")
            )
        );
    }
}
