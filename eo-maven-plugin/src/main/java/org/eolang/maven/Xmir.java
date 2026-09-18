/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import java.util.List;

/**
 * Parsing result as XMIR with possible errors.
 *
 * @since 0.60
 */
final class Xmir {

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
     *
     * @param res Resulting XML
     * @param errors List of errors
     */
    Xmir(final XML res, final List<String> errors) {
        this.res = res;
        this.errors = errors;
    }

    /**
     * Resulting XML.
     *
     * @return XML xmir representation of the EO object
     */
    XML xml() {
        return this.res;
    }

    /**
     * Is the XMIR broken.
     *
     * @return Is broken or not
     */
    boolean broken() {
        return !this.errors.isEmpty();
    }
}
