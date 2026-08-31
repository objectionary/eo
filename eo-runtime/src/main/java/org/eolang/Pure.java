/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Marker for a formation purify.xsl decided is safe to cache (see #5165).
 *
 * <p>A named formation nested inside another one is wrapped in
 * {@link PhSticky} right where {@code to-java.xsl} writes its constructor
 * call, because that call site is generated Java text the stylesheet
 * already controls. A top-level formation has no such site: it is loaded by
 * {@link PhPackage} through reflection, at a point that never sees the XMIR
 * {@code @pure} attribute the class came from. Implementing this marker on
 * the generated class carries that attribute across the reflection
 * boundary, so {@link PhPackage} can wrap an instance in {@link PhSticky}
 * itself once it sees the marker.</p>
 *
 * @since 0.75
 */
public interface Pure {
}
