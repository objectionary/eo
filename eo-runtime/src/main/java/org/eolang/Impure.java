/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks an atom whose result depends on the outside world.
 *
 * <p>An impure atom (file access, the operating system, the clock, randomness)
 * yields a different object every time it is calculated. Its φ-expression is
 * therefore not a reliable cache key, so caching decorators such as
 * {@code PhSticky} must never memoize it.</p>
 *
 * @since 0.60
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Impure {
}
