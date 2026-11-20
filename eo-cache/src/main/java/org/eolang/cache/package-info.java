/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * The cache shared by every build on this machine.
 *
 * <p>This package keeps the files a build step produces under one directory,
 * usually {@code ~/.eo}, so that the next build with the same sources takes
 * them from there instead of producing them again. A step asks
 * {@link org.eolang.cache.GlobalCache} how one file is to be written and gets
 * a {@link org.eolang.cache.Footprint} back. It lives in a module of its own,
 * so that any module of the compiler can cache its own step.</p>
 *
 * @since 0.77.0
 * @see <a href="https://www.eolang.org">Project site www.eolang.org</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.cache;
