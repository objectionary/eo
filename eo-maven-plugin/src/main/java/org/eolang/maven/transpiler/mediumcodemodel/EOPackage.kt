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
 * Represents an EO source code package with a fully qualified name.
 * @param packageName the fully qualified name of the package.
 */
class EOPackage(val packageName: String) : EOSourceEntity() {

    /**
     * Files that belong to the package.
     */
    val files = ArrayList<EOSourceFile>()

    /**
     * Add files that belong to the package.
     */
    fun addFile(vararg files: EOSourceFile) {
        this.files.addAll(files)
    }

    /**
     * Packages are not transpiled in the current implementation of the transpiler.
     */
    override fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>? {return null}
}