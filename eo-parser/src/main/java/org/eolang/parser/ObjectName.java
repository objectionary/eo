/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.function.Supplier;

/**
 * Object name.
 * @since 0.56.5
 */
@FunctionalInterface
public interface ObjectName extends Supplier<String> {
}
