/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2021 nlchar
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
package org.eolang.maven.transpiler.mediumcodemodel

import org.ainslec.picocog.PicoWriter

/**
 * Represent an EO source code entity
 * (it may be anything from a source file to an abstraction to a free attribute).
 */
abstract class EOSourceEntity {
    /**
     * Transpiles the source entity to the target code model (it may be Java files, classes or class fragments).
     * @param w Java code generation utility to write the target code to
     *          (may be absent for higher-order source entities that need to transpile their children only).
     * @return A list of target Java files generated through translation
     *         (may be absent for source entities that map to file fragments, not separate Java files).
     */
    abstract fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>?
}