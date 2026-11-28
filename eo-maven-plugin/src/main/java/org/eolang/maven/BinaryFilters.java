/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Set;

/**
 * Glob filters for binary placement and removal.
 *
 * @param includes Files eligible for placement
 * @param excludes Files excluded from placement
 * @param keeps Files preserved during removal
 * @since 0.64.0
 */
record BinaryFilters(Set<String> includes, Set<String> excludes, Set<String> keeps) {
}
