/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * If the attributes were not found in the Tojo.
 * @since 0.35.0
 */
final class AttributeNotFoundException extends RuntimeException {

    /**
     * Ctor.
     * @param attribute The attribute of Tojo.
     */
    AttributeNotFoundException(final TjsForeign.Attribute attribute) {
        super(String.format("There is no '%s' attribute in the tojo", attribute));
    }
}
