/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
/**
 * Object name package.
 * The main purpose of classes under the package is to provide a way to build the full name
 * of an EO object which consists of his identifier (QQ.io.stdout) and his hash.
 *  - {@link org.eolang.maven.name.OnVersioned} - default implementation that builds full name even
 *    if given identifier does not contain version
 *  - {@link org.eolang.maven.name.OnReplaced} - replaces tag in given identifier with corresponding
 *    hash
 *  - {@link org.eolang.maven.name.OnUnversioned} - builds object full name without a version
 *  - {@link org.eolang.maven.name.OnCached} - caching decorator
 *  - {@link org.eolang.maven.name.OnSwap} - behaves like one of encapsulated names depends on
 *    encapsulated condition
 */
package org.eolang.maven.name;
